use std::iter::{Chain, Peekable, Repeat, repeat};
use std::vec::IntoIter;

use lasso::{Key, Spur};
use rug::{Integer, Rational};

use crate::alias_resolution::{AliasFragment, AliasItem, AliasResolver};
use crate::ast::*;
use crate::lexer::StringPrefix;
use crate::source::{SourceMap, Span};
use crate::token::{ResolvedInterner, Token, TokenKind};

type TokenStream = Peekable<Chain<IntoIter<Token>, Repeat<Token>>>;

#[derive(Debug)]
pub struct Parser<'r> {
    tokens: TokenStream,
    current_token: Token,
    alias_resolver: &'r mut AliasResolver
}


impl<'r> Parser<'r> {
    pub const MAX_ARGS: usize = 255;

    pub fn new(tokens: Vec<Token>, alias_resolver: &'r mut AliasResolver) -> Self {
        let last = *tokens.last().unwrap();
        let mut t = tokens
            .into_iter()
            .chain(repeat(last))
            .peekable();
        let current = t.next().unwrap();

        Self {
            tokens: t,
            current_token: current,
            alias_resolver
        }
    }

    pub fn parse(mut self, source_map: &mut SourceMap, interner: &ResolvedInterner) -> Vec<Stmt> {
        let mut stmts = vec![];

        while !self.at_end() {
            if self.accept(TokenKind::Semicolon) {
                continue
            }            

            stmts.push(self.parse_stmt(source_map, interner));
        }

        stmts
    }

    fn parse_stmt(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Stmt {
        if self.starts_non_expr_stmt() {
            self.parse_non_expr_stmt(source_map, interner)
        } else {
            self.parse_expr_stmt(source_map, interner)
        }
    }

    fn starts_non_expr_stmt(&self) -> bool {
        matches!(self.current_kind(), 
              TokenKind::Let
            | TokenKind::Var
            | TokenKind::Const
            | TokenKind::Fn
            | TokenKind::Sym
            | TokenKind::Context
            | TokenKind::Enum
            | TokenKind::Struct
            | TokenKind::Type
            | TokenKind::Macro
            | TokenKind::Alias
            | TokenKind::Using
            | TokenKind::For
            | TokenKind::While
        )
    }

    fn parse_non_expr_stmt(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Stmt {
        let token = self.current();
        self.advance();

        match token.kind() {
            TokenKind::Let => self.parse_let(token.span().start(), source_map, false, interner),
            TokenKind::Var => self.parse_var(token.span().start(), source_map, false, interner),
            TokenKind::Const => self.parse_const(token.span().start(), source_map, false, interner),
            TokenKind::Fn => self.parse_fn(token.span().start(), source_map, false, interner),
            TokenKind::Sym => self.parse_sym(token.span().start(), source_map, interner),
            TokenKind::Context => todo!(),
            TokenKind::Enum => self.parse_enum(token.span().start(), source_map, interner),
            TokenKind::Struct => self.parse_struct(token.span().start(), source_map, interner),
            TokenKind::Type => self.parse_type_def(token.span().start(), source_map, interner),
            TokenKind::Alias => self.parse_alias(token.span().start(), source_map, interner),
            TokenKind::Using => todo!(),
            TokenKind::For => todo!(),
            TokenKind::While => todo!(),
            _ => unreachable!()
        }
    }

    /// `let` should have already been accepted
    fn parse_let(&mut self, span_start: usize, source_map: &SourceMap, in_expr: bool, interner: &ResolvedInterner) -> Stmt {
        let bindings = self.parse_bindings(source_map, interner);
        let (kind, value) = if self.accept(TokenKind::Eq) {
            (LetKind::Assign, Some(self.parse_expr(source_map, interner, 0)))
        } else if self.accept(TokenKind::ColonEq) {
            (LetKind::Define, Some(self.parse_expr(source_map, interner, 0)))
        } else {
            (LetKind::Declare, None)
        };

        if self.accept(TokenKind::In) {
            let expr = self.parse_expr(source_map, interner, 0);

            let span_end = if in_expr {
                expr.span().end()
            } else {
                let span_end = self.current().span().end();
                self.expect(TokenKind::Semicolon);
                
                span_end
            };

            Stmt::Expr {
                span: Span::new(span_start, span_end, self.current().span().source_id()),
                expr: Expr::LetIn {
                    span: Span::new(span_start, expr.span().end(), expr.span().source_id()),
                    def: Box::new(Let::new(bindings, kind, value)),
                    expr: Box::new(expr)
                }
            }
        } else if in_expr {
            todo!("expected 'in'")
        } else {
            let span_end = self.current().span().end();
            self.expect(TokenKind::Semicolon);

            Stmt::Let {
                span: Span::new(span_start, span_end, self.current().span().source_id()),
                def: Let::new(bindings, kind, value)
            }
        }
    }

    fn parse_bindings(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Vec<Binding> {
        let mut bindings = vec![];

        loop {
            bindings.push(self.parse_binding(source_map, interner));

            if self.accept(TokenKind::Comma) {
                continue
            } else {
                break
            }
        }

        bindings
    }

    // TODO: Record, Tuple, Destructuring, Rest, and _ bindings, and distinguishing Tuple Constructor from Function Call
    fn parse_binding(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Binding {
        let mut name: Var = self.require(TokenKind::Ident)
            .unwrap_or_else(|| todo!("identifier expected"))
            .try_into()
            .unwrap();
        self.alias_resolver.resolve_var_to_var(&mut name);
        let span_start = name.span().start();

        if let Some("<") = self.current_op(source_map) {
            Binding::Fn(self.finish_header(source_map, name, span_start, interner))
        } else if let TokenKind::LParen = self.current_kind() {
            Binding::Call(self.finish_header(source_map, name, span_start, interner))
        } else {
            Binding::Name(name, self.parse_type_annotation(source_map, interner))
        }
    }

    fn parse_var(&mut self, span_start: usize, source_map: &SourceMap, in_expr: bool, interner: &ResolvedInterner) -> Stmt {                
        let name  = self.require_ident_and_resolve_alias();
        self.advance();

        let ty = self.parse_type_annotation(source_map, interner);
        let def = if self.accept(TokenKind::Eq) {
            Some(self.parse_expr(source_map, interner, 0))
        } else { None };

        if self.accept(TokenKind::In) {
            let expr = self.parse_expr(source_map, interner, 0);

            let span_end = if in_expr {
                expr.span().end()
            } else {
                let span_end = self.current().span().end();
                self.expect(TokenKind::Semicolon);
                
                span_end
            };

            let var_in_expr = Expr::VarIn {
                span: Span::new(span_start, expr.span().end(), expr.span().source_id()),
                name,
                ty,
                value: def.map(Box::new),
                expr: Box::new(expr)
            };

            Stmt::Expr {
                span: Span::new(var_in_expr.span().start(), span_end, var_in_expr.span().source_id()),
                expr: var_in_expr
            }
        } else if in_expr {
            todo!("expected `in`")
        } else {
            let span_end = self.current().span().end();
            self.expect(TokenKind::Semicolon);

            Stmt::Var {
                span: Span::new(span_start, span_end, self.current().span().source_id()),
                name,
                ty,
                value: def
            }
        }
    }

    fn parse_const(&mut self, span_start: usize, source_map: &SourceMap, in_expr: bool, interner: &ResolvedInterner) -> Stmt {                
        let name = self.require_ident_and_resolve_alias();
        self.advance();

        let ty = self.parse_type_annotation(source_map, interner);
        let def = if self.accept(TokenKind::Eq) {
            Some(self.parse_expr(source_map, interner, 0))
        } else { None };

        if self.accept(TokenKind::In) {
            let expr = self.parse_expr(source_map, interner, 0);

            let span_end = if in_expr {
                expr.span().end()
            } else {
                let span_end = self.current().span().end();
                self.expect(TokenKind::Semicolon);
                
                span_end
            };

            let const_in_expr = Expr::ConstIn {
                span: Span::new(span_start, expr.span().end(), expr.span().source_id()),
                name,
                ty,
                value: def.map(Box::new),
                expr: Box::new(expr)
            };

            Stmt::Expr {
                span: Span::new(const_in_expr.span().start(), span_end, const_in_expr.span().source_id()),
                expr: const_in_expr
            }
        } else if in_expr {
            todo!("expected `in`")
        } else {
            let span_end = self.current().span().end();
            self.expect(TokenKind::Semicolon);

            Stmt::Const {
                span: Span::new(span_start, span_end, self.current().span().source_id()),
                name,
                ty,
                value: def
            }
        }
    }

    fn parse_fn(&mut self, span_start: usize, source_map: &SourceMap, in_expr: bool, interner: &ResolvedInterner) -> Stmt {
        let header = self.parse_header(source_map, interner);
        let (span_end, value) = if self.accept(TokenKind::Eq) {
            let expr = self.parse_expr(source_map, interner, 0);

            let span_end = if in_expr {
                expr.span().end()
            } else {
                let span_end = self.current().span().end();
                self.expect(TokenKind::Semicolon);

                span_end
            };

            (span_end, expr)
        } else if let TokenKind::LBrace = self.current_kind() {
            let block = self.parse_block(self.current(), source_map, interner);

            (block.span().end(), block)
        } else {
            todo!("expected block for fn def");
        };

        if self.accept(TokenKind::In) {
            let expr = self.parse_expr(source_map, interner, 0);

            let span_end = if in_expr {
                expr.span().end()
            } else {
                let span_end = self.current().span().end();
                self.expect(TokenKind::Semicolon);

                span_end
            };

            let fn_in_expr = Expr::FnIn {
                span: Span::new(span_start, expr.span().end(), expr.span().source_id()),
                header,
                value: Box::new(value),
                expr: Box::new(expr) 
            };

            Stmt::Expr {
                span: Span::new(fn_in_expr.span().start(), span_end, fn_in_expr.span().source_id()),
                expr: fn_in_expr
            }
        } else if in_expr {
            todo!("expected `in`")
        } else {
            Stmt::Fn {
                span: Span::new(span_start, span_end, self.current().span().source_id()),
                header,
                value,
            }
        }
    }

    fn parse_header(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> FnHeader {
        let mut name: Var = self.require(TokenKind::Ident)
            .unwrap_or_else(|| todo!("identifier expected"))
            .try_into()
            .unwrap();
        self.alias_resolver.resolve_var_to_var(&mut name);
        let span_start = name.span().start();

        self.finish_header(source_map, name, span_start, interner)
    }

    fn finish_header(&mut self, source_map: &SourceMap, name: Var, span_start: usize, interner: &ResolvedInterner) -> FnHeader {
        let ty_args = self.parse_generic(source_map);
        let (args, kwargs, args_span_end) = self.parse_args_def(source_map, interner);
        let ty = self.parse_type_annotation(source_map, interner);

        let span_end = if let Some(ref ty) = ty {
            ty.span().end()
        } else {
            args_span_end
        };

        FnHeader::new(
            name,
            ty_args,
            args,
            kwargs,
            ty,
            Span::new(span_start, span_end, self.current().span().source_id())
        )
    }

    /// Parses an args definition, meaning args, kwargs, and any type annotations for them. It returns args, kwargs, and the span end of the whole definition.
    fn parse_args_def(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> (Vec<(Var, Option<Type>)>, Vec<(Var, Option<Type>)>, usize) {
        let mut args = vec![];
        let mut kwargs = vec![];
        let mut in_kwargs = false;

        self.expect(TokenKind::LParen);
        
        if let Some(rp) = self.take(TokenKind::RParen) {
            return (args, kwargs, rp.span().end());
        }

        loop {
            let arg = self.require_ident_and_resolve_alias();
            let ty = self.parse_type_annotation(source_map, interner);
            
            if in_kwargs {
                kwargs.push((arg, ty));
            } else {
                args.push((arg, ty));
            }

            // start kwargs section
            if matches!(self.current_kind(), TokenKind::Semicolon) && !in_kwargs {
                self.advance();
                in_kwargs = true;

                if self.accept(TokenKind::RParen) {
                    todo!("expected keyword arguments after ';'")
                }
            // already in kwargs section
            } else if let TokenKind::Semicolon = self.current_kind() {
                todo!("only one ';' allowed in argument definition to separate args from keyword args")
            
            } else {
                if let Some(rp) = self.take(TokenKind::RParen) {
                    return (args, kwargs, rp.span().end())
                } else if self.expect(TokenKind::Comma) {
                    if let Some(rp) = self.take(TokenKind::RParen) {
                        return (args, kwargs, rp.span().end())
                    }
                }
            }
        }
    }

    /// Attempts to parse a type annotation. Either it will be a regular type annotation, or an implicit refinement type annotation, or there may be no type annotation, in which it will output None.
    fn parse_type_annotation(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Option<Type> {
        // : Type
        if self.accept_op(source_map, ":") {
            Some(self.parse_type(source_map, interner))
        // :: Implicit Refinement
        } else if self.accept_op(source_map, "::") {
            todo!()
        } else {
            None
        }
    }

    fn parse_sym(&mut self, span_start: usize, source_map: &SourceMap, interner: &ResolvedInterner) -> Stmt {
        let name = self.require_ident_and_resolve_alias();
        let mut args = if let TokenKind::LParen = self.current_kind() {
            let (args, kwargs, _) = self.parse_args_def(source_map, interner);
            if !kwargs.is_empty() {
                todo!("keyword-only arguments are not allowed in a symbolic node definition");
            }

            args
        } else {
            vec![]
        };

        let ty = self.parse_type_annotation(source_map, interner);

        let Some(semi) = self.require(TokenKind::Semicolon)
        else { todo!("expected semicolon") };

        Stmt::Sym {
            name,
            args,
            ty,
            span: Span::new(span_start, semi.span().end(), semi.span().source_id())
        }
    }

    fn parse_enum(&mut self, span_start: usize, source_map: &SourceMap, interner: &ResolvedInterner) -> Stmt {
        let name = self.require_ident_and_resolve_alias();
        let ty_args = self.parse_generic(source_map);

        self.expect(TokenKind::LBrace);
        if let Some(rb) = self.take(TokenKind::RBrace) {
            return Stmt::Enum {
                name,
                ty_args,
                variants: vec![],
                span: Span::new(span_start, rb.span().end(), rb.span().source_id()),
            };
        }

        let mut variants = vec![];
        loop {
            let tag = self.require_ident_and_resolve_alias();

            if self.accept(TokenKind::LParen) {
                if self.accept(TokenKind::RParen) {
                    todo!("empty enum tuple variants are not allowed")
                }

                let mut data = vec![];
                loop {
                    data.push(self.parse_type(source_map, interner));

                    if self.accept(TokenKind::RParen)
                        || (self.expect(TokenKind::Comma) && self.accept(TokenKind::RParen)) {
                        break
                    }
                }

                variants.push(Variant::Tuple(data));
            } else if self.accept(TokenKind::LBrace) {
                if self.accept(TokenKind::RBrace) {
                    todo!("empty enum record variants are not allowed")
                }

                let mut entries = vec![];
                loop {
                    let key = self.require_ident_and_resolve_alias();

                    self.expect_op(source_map, ":");
                    let ty = self.parse_type(source_map, interner);

                    entries.push((key, ty));

                    if self.accept(TokenKind::RBrace)
                        || (self.expect(TokenKind::Comma) && self.accept(TokenKind::RBrace)) {
                        break
                    }
                }

                variants.push(Variant::Record(entries));
            } else {
                variants.push(Variant::Const(tag));
            }
            
            if let Some(rb) = self.take(TokenKind::RBrace) {
                return Stmt::Enum {
                    name,
                    ty_args,
                    variants,
                    span: Span::new(span_start, rb.span().end(), rb.span().source_id()),
                }
            } else if self.expect(TokenKind::Comma) {
                if let Some(rb) = self.take(TokenKind::RBrace) {
                    return Stmt::Enum {
                        name,
                        ty_args,
                        variants,
                        span: Span::new(span_start, rb.span().end(), rb.span().source_id()),
                    }
                }
            }
        }
    }

    fn parse_struct(&mut self, span_start: usize, source_map: &SourceMap, interner: &ResolvedInterner) -> Stmt {
        let name = self.require_ident_and_resolve_alias();
        let ty_args = self.parse_generic(source_map);

        self.expect(TokenKind::LBrace);

        if let Some(rb) = self.take(TokenKind::RBrace) {
            return Stmt::Struct {
                name,
                ty_args,
                fields: vec![],
                span: Span::new(span_start, rb.span().end(), rb.span().source_id()),
            };
        }

        let mut fields = vec![];
        loop {
            let field = self.require_ident_and_resolve_alias();

            self.expect_op(source_map, ":");
            let ty = self.parse_type(source_map, interner);

            fields.push((field, ty));

            if let Some(rb) = self.take(TokenKind::RBrace) {
                return Stmt::Struct {
                    name,
                    ty_args,
                    fields,
                    span: Span::new(span_start, rb.span().end(), rb.span().source_id())
                }
            } else if self.expect(TokenKind::Comma) {
                if let Some(rb) = self.take(TokenKind::RBrace) {
                    return Stmt::Struct {
                        name,
                        ty_args,
                        fields,
                        span: Span::new(span_start, rb.span().end(), rb.span().source_id())
                    }
                }
            }
        }
    }

    fn parse_type_def(&mut self, span_start: usize, source_map: &SourceMap, interner: &ResolvedInterner) -> Stmt {
        let name = self.require_ident_and_resolve_alias();
        let ty_args = self.parse_generic(source_map);

        // TODO: abstract type (abstract type T; ????), just type (type T;) declarations
        self.expect(TokenKind::Eq);

        let def = self.parse_type(source_map, interner);
        
        let Some(semi) = self.require(TokenKind::Semicolon)
        else { todo!("expected semicolon") };

        Stmt::Type {
            name,
            ty_args,
            def,
            span: Span::new(span_start, semi.span().end(), semi.span().source_id())
        }
    }

    /// Parses alias definition of the form `alias NEW for OLD;`.
    fn parse_alias(&mut self, span_start: usize, source_map: &SourceMap, interner: &ResolvedInterner) -> Stmt {
        let new = match self.current_kind() {
            TokenKind::Ident => {
                let ident = AliasLeft::Ident(self.current().try_into().unwrap());
                self.advance();

                ident
            }

            TokenKind::Operator => {
                let op = AliasLeft::Oper(Oper::try_from(self.current()).unwrap());
                self.advance();

                op
            }

            _ => todo!("expected identifier or operator")
        };

        self.expect(TokenKind::For);

        let old = match self.current_kind() {
            TokenKind::Ident => {
                let ident = AliasRight::Ident(self.current().try_into().unwrap());
                self.advance();

                ident
            }

            TokenKind::Operator => {
                let op = AliasRight::Oper(Oper::try_from(self.current()).unwrap());
                self.advance();
                
                op
            }

            TokenKind::Backtick => AliasRight::OpLit(self.parse_operator_literal(source_map, interner)),

            TokenKind::LParen => {
                let lp = self.current();
                self.advance();

                let mut expr = self.parse_expr(source_map, interner, 0);
                let Some(rp) = self.require(TokenKind::RParen)
                else { todo!("expected ')'") };

                expr.span_mut().set_start(lp.span().start());
                expr.span_mut().set_end(rp.span().end());

                AliasRight::Expr(expr)
            }

            _ => todo!("expected identifier, operator, operator literal, or expression surrounded with parentheses")
        };

        let Some(semi) = self.require(TokenKind::Semicolon)
        else { todo!("expected ';'") };

        let new_item = AliasItem::from(&new);
        let old_item = AliasItem::from(&old);
        self.alias_resolver.register_alias(new_item, old_item);

        Stmt::Alias {
            new,
            old,
            span: Span::new(span_start, semi.span().end(), semi.span().source_id())
        }
    }

    /// Outputs empty vector if no generic arguments are seen. 
    fn parse_generic(&mut self, source_map: &SourceMap) -> Vec<Generic> {
        if !self.accept_op(source_map, "<") {
            return vec![]
        }
        
        let mut args = vec![];

        if self.accept_op(source_map, ">") {
            return args;
        }

        loop {
            if let Some(name) = self.require(TokenKind::Ident) {
                let mut name = Var::try_from(name).unwrap();
                self.alias_resolver.resolve_var_to_var(&mut name);

                if self.accept_op(source_map, ":") {
                    // TODO: do parsing of valid rhs of sat
                    // let sat = self.parse_sat(source_map);

                    args.push(Generic { name })
                } else {
                    args.push(Generic { name });
                }

                // TODO: perform >> splitting
                if self.accept_op(source_map, ">")
                    || (self.expect(TokenKind::Comma) && self.accept_op(source_map, ">")) {
                    break
                }
            } else {
                todo!()
            }
        }

        args
    }

    fn parse_type(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Type {        
        self.parse_exponential_type(source_map, interner)
    }

    fn parse_exponential_type(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Type {
        let ty = self.parse_primary_type(source_map, interner);

        if self.accept_op(source_map, "^") {
            let exponent = self.parse_lit(self.current(), source_map, interner);

            if let Expr::Int {..} = exponent {
                Type::Exponent {
                    span: Span::new(ty.span().start(), exponent.span().end(), exponent.span().source_id()),
                    ty: Box::new(ty),
                    exp: Box::new(exponent)
                }
            } else {
                todo!("type exponential can only contain Natural exponents")
            }
        } else {
            ty
        }
    }

    fn parse_primary_type(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Type {
        match self.current_kind() {
            TokenKind::LBracket => self.parse_array_type(source_map, interner),
            TokenKind::LParen => self.parse_grouping_type(source_map, interner),
            TokenKind::Ident => {
                let mut ident = self.current().try_into().unwrap();
                self.alias_resolver.resolve_var_to_var(&mut ident);
                self.advance();

                Type::Named(ident)
            }
            _ => todo!()
        }
    }

    fn parse_array_type(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Type {
        let span_start = self.current().span().start();
        self.accept(TokenKind::LBracket);

        let shape = if self.accept(TokenKind::RBracket) {
            Shape::Empty
        } else if self.accept_op(source_map, "*") {
            if self.accept(TokenKind::RBracket) {
                Shape::Dynamic
            } else {
                todo!("multirank arrays cannot have dynamic shape")
            }
        } else {
            let mut shape_specs = vec![];

            macro_rules! finish_parsing_item {
                () => {
                    if self.accept(TokenKind::RBracket) {
                        break
                    } else {
                        self.expect(TokenKind::Comma);

                        if self.accept(TokenKind::RBracket) {
                            break
                        }
                    }
                };
            }

            loop {
                if self.accept_op(source_map, "?") {
                    shape_specs.push(ShapeSpec::Unknown);

                    finish_parsing_item!()
                } else if self.accept_op(source_map, "*") {
                    todo!("multirank arrays cannot have dynamic shape")
                } else {
                    let expr = self.parse_lit(self.current(), source_map, interner);

                    match expr {
                        Expr::Ident(_)   |
                        Expr::Int { .. } => (),
                        _ => todo!("array shape indicator can only hold an unknown qualifier ('?'), a whole number, or an identifier")
                    }

                    shape_specs.push(ShapeSpec::Known(expr));
                    finish_parsing_item!()
                }
            }

            Shape::Specified(shape_specs)
        };

        let ty = self.parse_type(source_map, interner);

        Type::Array {
            span: Span::new(span_start, ty.span().end(), ty.span().source_id()),
            shape,
            ty: Box::new(ty)
        }
    }

    fn parse_grouping_type(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Type {
        let span_start = self.current().span().start();
        self.accept(TokenKind::LParen);

        if let Some(rp) = self.take(TokenKind::RParen) {
            Type::Unit {
                span: Span::new(span_start, rp.span().end(), rp.span().source_id())
            }
        } else {
            let ty = self.parse_type(source_map, interner);

            if let Some(rp) = self.take(TokenKind::RParen) {
                Type::Grouping {
                    ty: Box::new(ty),
                    span: Span::new(span_start, rp.span().end(), rp.span().source_id())
                }
            } else {
                self.expect(TokenKind::Comma);

                let mut types = if let Type::Exponent { ty: lhs, exp, span: exp_span} = &ty {
                    if let Expr::Int { value: AstInt::Small(value), .. } = Box::as_ref(exp) {
                        if *value > Self::MAX_ARGS as u32 {
                            todo!("too high of a type exponent")
                        } else if value < &0 {
                            todo!("type exponents must be a natural number")
                        } else {
                            let mut types = vec![];
                            for _ in 0..*value {
                                let mut lhs = *lhs.to_owned();
                                *lhs.span_mut() = *exp_span;

                                types.push(lhs);
                            }

                            types
                        }
                    } else {
                        todo!("type exponents must be naturals")
                    }
                } else {
                    vec![ty]
                };

                if let Some(rp) = self.take(TokenKind::RParen) {
                    return Type::Tuple {
                        types,
                        span: Span::new(span_start, rp.span().end(), rp.span().source_id())
                    }
                }

                loop {
                    let ty = self.parse_type(source_map, interner);

                    if let Type::Exponent { ty: lhs, exp, span: exp_span } = &ty {
                        if let Expr::Int {  value: AstInt::Small(value), .. } = Box::as_ref(exp) {
                            if *value > Self::MAX_ARGS as u32 {
                                todo!("too high of a type exponent")
                            } else if *value < 0 {
                                todo!("type exponents must be a natural number")
                            } else {
                                for _ in 0..*value {
                                    let mut lhs = *lhs.to_owned();
                                    *lhs.span_mut() = *exp_span;

                                    types.push(lhs);
                                }
                            }
                        } else {
                            unreachable!("should be unreachable")
                        }
                    } else {
                        types.push(ty);
                    }

                    if let Some(rp) = self.take(TokenKind::RParen) {
                        break Type::Tuple {
                            types,
                            span: Span::new(span_start, rp.span().end(), rp.span().source_id())
                        }
                    } else {
                        self.expect(TokenKind::Comma);

                        if let Some(rp) = self.take(TokenKind::RParen) {
                            break Type::Tuple {
                                types,
                                span: Span::new(span_start, rp.span().end(), rp.span().source_id())
                            }
                        }    
                    }
                }
            }
        }
    }

    fn parse_expr_stmt(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Stmt {
        let expr = self.parse_expr(source_map, interner, 0);

        if let Some(semi) = self.take(TokenKind::Semicolon) {
            Stmt::Expr {
                span: Span::new(expr.span().start(), semi.span().end(), semi.span().source_id()),
                expr
            }
        } else {
            todo!("report error - expected semicolon")
        }
    }

    /// A Pratt Parser for expressions
    fn parse_expr(&mut self, source_map: &SourceMap, interner: &ResolvedInterner, binding_power: u32) -> Expr {
        let mut token = self.advance();

        let mut lhs = match self.get_resolved_token_entry(token, source_map, interner) {
            (tok, EntryOrExpr::Entry(entry)) => {
                token = tok;

                if entry.nud.is_none() {
                    todo!("Expected expression, found {:?}", token.kind())
                }

                entry.nud.unwrap()(self, token, source_map, interner)
            }

            (tok, EntryOrExpr::Expr(expr)) => {
                token = tok;

                expr
            }
        };
        
        let mut led = None; // will be replaced or unused
        while {
            token = self.current();
            binding_power < match self.get_resolved_token_entry(token, source_map, interner) {
                (tok, EntryOrExpr::Entry(entry)) => {
                    token = tok;
                    
                    self.advance();

                    if entry.led.is_none() {
                        todo!("Expected binary operator, found {:?}", self.current())
                    }

                    led = entry.led;
                    entry.led_prec
                }

                (tok, EntryOrExpr::Expr(_)) => {
                    token = tok;

                    0 // essentially break
                }
            }
        } {
            lhs = led.unwrap()(self, token, lhs, source_map, interner);
        }

        lhs
    }

    fn get_resolved_token_entry(&mut self, token: Token, source_map: &SourceMap, interner: &ResolvedInterner) -> (Token, EntryOrExpr<'r>) {
        match token.kind() {
            TokenKind::Ident => {
                let frag = self.alias_resolver.resolve_var(&mut token.try_into().unwrap());

                match frag {
                    AliasFragment::Ident(name) => (
                        name.synth_token(), 
                        EntryOrExpr::Entry(Self::get_operator_entry(token, OperatorKey::Kind(TokenKind::Ident), source_map))
                    ),
                    AliasFragment::Oper(oper) => (
                        token, 
                        EntryOrExpr::Entry(Self::get_operator_entry_inner(OperatorKey::Oper(oper.get_lexeme(source_map))))
                    ),
                    AliasFragment::OpLit(oplit) => todo!(),
                    AliasFragment::Expr(expr) => (token, EntryOrExpr::Expr(expr))
                }
            }  

            TokenKind::Operator => {
                let frag = self.alias_resolver.resolve_oper(&mut token.try_into().unwrap());

                match frag {
                    AliasFragment::Ident(_) => unreachable!(),
                    AliasFragment::Oper(_) => (
                        token, 
                        EntryOrExpr::Entry(Self::get_operator_entry(token, OperatorKey::Kind(TokenKind::Operator), source_map))
                    ),
                    AliasFragment::OpLit(oplit) => todo!(),
                    AliasFragment::Expr(_) => unreachable!()
                }
            }

            TokenKind::Backtick => todo!(),

            _ => (
                token, 
                EntryOrExpr::Entry(Self::get_operator_entry(token, OperatorKey::Kind(token.kind()), source_map))
            )
        }
    }
    
    fn get_operator_entry(token: Token, key: OperatorKey, source_map: &SourceMap) -> OperatorEntry<'r> {
        match key {
            OperatorKey::Kind(TokenKind::Operator) => {
                let oper: Oper = token.try_into().unwrap();
                Self::get_operator_entry_inner(OperatorKey::Oper(oper.get_lexeme(source_map)))
            }
            _ => todo!()
        }
    }

    fn get_operator_entry_inner(key: OperatorKey) -> OperatorEntry<'r> {
        use OperatorKey::*;
        use TokenKind::*;

        match key {
            Kind(LParen) => OperatorEntry      { nud: Some(Self::parse_grouping),  nud_prec: Prec::Group.bp(), led: Some(Self::parse_call),    led_prec: Prec::Call.bp() },
            Kind(RParen) => OperatorEntry      { nud: None,                        nud_prec: 0,                led: None,                      led_prec: 0 },
            Kind(LBracket) => OperatorEntry    { nud: Some(Self::parse_lit),       nud_prec: 0,                led: None,                      led_prec: 0 },
            Kind(RBracket) => OperatorEntry    { nud: None,                        nud_prec: 0,                led: None,                      led_prec: 0 },
            Kind(LBrace) => OperatorEntry      { nud: Some(Self::parse_block),     nud_prec: Prec::Group.bp(), led: None,  /* record lit? */   led_prec: 0 },
            Kind(RBrace) => OperatorEntry      { nud: None,                        nud_prec: 0,                led: None,                      led_prec: 0 },
            Kind(Semicolon) => OperatorEntry   { nud: None,                        nud_prec: 0,                led: None,                      led_prec: 0 },
            Kind(Comma) => OperatorEntry       { nud: None,                        nud_prec: 0,                led: None,                      led_prec: 0 },
            Kind(Dot) => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_accessor), led_prec: Prec::Access.bp() },
            Kind(Ident) => OperatorEntry { nud: Some(Self::parse_lit),       nud_prec: 0,                led: None,                      led_prec: 0 },
            Kind(Int) => OperatorEntry         { nud: Some(Self::parse_lit),       nud_prec: 0,                led: None,                      led_prec: 0 },
            Kind(Real) => OperatorEntry        { nud: Some(Self::parse_lit),       nud_prec: 0,                led: None,                      led_prec: 0 },
            Kind(Imag) => OperatorEntry        { nud: Some(Self::parse_lit),       nud_prec: 0,                led: None,                      led_prec: 0 },
            Kind(StringStart) => OperatorEntry { nud: Some(Self::parse_lit), nud_prec: 0, led: None, led_prec: 0 },
            Kind(Let) => OperatorEntry { nud: Some(Self::parse_def_in), nud_prec: 0, led: None, led_prec: 0 },
            Kind(Var) => OperatorEntry { nud: Some(Self::parse_def_in), nud_prec: 0, led: None, led_prec: 0 },
            Kind(Const) => OperatorEntry { nud: Some(Self::parse_def_in), nud_prec: 0, led: None, led_prec: 0 },
            Kind(Fn) => OperatorEntry { nud: Some(Self::parse_def_in), nud_prec: 0, led: None, led_prec: 0 },
            Kind(For) => OperatorEntry { nud: None, nud_prec: 0, led: None, led_prec: 0 },
            Kind(While) => OperatorEntry { nud: None, nud_prec: 0, led: None, led_prec: 0 },
            Kind(If) => OperatorEntry { nud: Some(Self::parse_if), nud_prec: 0, led: None, led_prec: 0 },
            Kind(Else) => OperatorEntry { nud: None, nud_prec: 0, led: None, led_prec: 0 },
            Kind(Match) => OperatorEntry { nud: Some(Self::parse_match), nud_prec: 0, led: None, led_prec: 0 },
            Kind(And) => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_and), led_prec: Prec::And.bp() },
            Kind(Or) => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_or), led_prec: Prec::Or.bp() },
            Kind(Xor) => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_xor), led_prec: Prec::Xor.bp() },
            Kind(Not) => OperatorEntry { nud: Some(Self::parse_not), nud_prec: Prec::Unary.bp(), led: None, led_prec: 0 },
            Kind(Is) => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_is), led_prec: Prec::Is.bp() },
            Kind(As) => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_as), led_prec: Prec::As.bp() },
            Kind(SlashIn) => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_membership), led_prec: Prec::Comparison.bp() },
            Kind(SlashNotIn) => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_membership), led_prec: Prec::Comparison.bp() },
            Kind(Eq) => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_assign), led_prec: Prec::Assign.bp() },
            Oper("∈") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_membership), led_prec: Prec::Comparison.bp() },
            Oper("∉") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_membership), led_prec: Prec::Comparison.bp() },
            Oper("+") => OperatorEntry { nud: Some(Self::parse_builtin_unary), nud_prec: Prec::Unary.bp(), led: Some(Self::parse_additive), led_prec: Prec::Additive.bp() },
            Oper("+=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_assign), led_prec: Prec::Assign.bp() },
            Oper("-") => OperatorEntry { nud: Some(Self::parse_builitin_unary), nud_prec: Prec::Unary.bp(), led: Some(Self::parse_additive), led_prec: Prec::Additive.bp() },
            Oper("-=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_assign), led_prec: Prec::Assign.bp() },
            Oper("+-") => OperatorEntry { nud: Some(Self::parse_builtin_unary), nud_prec: Prec::Unary.bp(), led: Some(Self::parse_additive), led_prec: Prec::Additive.bp() },
            Oper("+-=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(parse_assign), led_prec: Prec::Assign.bp() },
            Oper("-+") => OperatorEntry { nud: Some(Self::parse_builtin_unary), nud_prec: Prec::Unary.bp(), led: Some(Self::parse_additive), led_prec: Prec::Additive.bp() },
            Oper("-+=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_assign), led_prec: Prec::Assign.bp() },
            Oper("*") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_multiplicative), led_prec: Prec::Multiplicative.bp() },
            Oper("*=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_assign), led_prec: Prec::Assign.bp() },
            Oper("/") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_multiplicative), led_prec: Prec::Multiplicative.bp() },
            Oper("/=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_assign), led_prec: Prec::Assign.bp() },
            Oper("//") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_multiplicative), led_prec: Prec::Multiplicative.bp() },
            Oper("//=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_assign), led_prec: Prec::Assign.bp() },
            Oper("%") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_multiplicative), led_prec: Prec::Multiplicative.bp() },
            Oper("%=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_assign), led_prec: Prec::Assign.bp() },
            Oper("^") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_exponentative), led_prec: Prec::Exponentative.bp() },
            Oper("^=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_assign), led_prec: Prec::Assign.bp() },
            Oper("|") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_pipe), led_prec: Prec::Lowest.bp() },
            Oper("==") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_comparison), led_prec: Prec::Comparison.bp() },
            Oper("!=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_comparison), led_prec: Prec::Comparison.bp() },
            Oper("<") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_comparison), led_prec: Prec::Comparison.bp() },
            Oper("<=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_comparison), led_prec: Prec::Comparison.bp() },
            Oper(">") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_comparison), led_prec: Prec::Comparison.bp() },
            Oper(">=") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_comparison), led_prec: Prec::Comparison.bp() },
            Oper("...") => OperatorEntry { nud: Some(Self::parse_spread), nud_prec: Prec::Lowest.bp(), led: None, led_prec: 0 },
            // TODO: ranges
            Oper("@") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_composition), led_prec: Prec::Composition.bp() },
            Oper("->") => OperatorEntry { nud: None, nud_prec: 0, led: Some(Self::parse_lambda), led_prec: Prec::Lambda.bp() },
            Kind(Operator) => unreachable!(),
            _ => todo!()
        }
    }

    fn parse_grouping(&mut self, token: Token, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        todo!()
    }

    fn parse_if(&mut self, token: Token, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        let cond = Box::new(self.parse_expr(source_map, interner, 0));

        let Some(lb) = self.take(TokenKind::LBrace)
        else { todo!("expected '{{'") };
        let if_body = Box::new(self.parse_block(token, source_map, interner));

        let Some(rb) = self.take(TokenKind::RBrace)
        else { todo!("expected '{{'") };

        if let TokenKind::Else = self.current_kind() {
            self.advance();
            let else_body = self.parse_expr(source_map, interner, 0);

            Expr::If {
                span: Span::new(token.span().start(), else_body.span().end(), else_body.span().source_id()),
                cond,
                if_body,
                else_body: Some(Box::new(else_body))
            }
        } else {
            Expr::If {
                cond,
                if_body,
                else_body: None,
                span: Span::new(token.span().start(), rb.span().end(), rb.span().source_id())
            }
        }
    }

    fn parse_match(&mut self, token: Token, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        todo!()
    }

    fn parse_and(&mut self, token: Token, lhs: Expr, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        todo!()
    }

    fn parse_or(&mut self, token: Token, lhs: Expr, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        todo!()
    }

    fn parse_xor(&mut self, token: Token, lhs: Expr, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        todo!()
    }

    fn parse_not(&mut self, token: Token, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        todo!()
    }

    fn parse_is(&mut self, token: Token, lhs: Expr, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        todo!()
    }
    
    fn parse_as(&mut self, token: Token, lhs: Expr, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        todo!()
    }

    fn parse_membership(&mut self, token: Token, lhs: Expr, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        todo!()
    }

    fn parse_call(&mut self, token: Token, lhs: Expr, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        todo!()
    }

    fn parse_accessor(&mut self, token: Token, lhs: Expr, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {

    }

    fn parse_def_in(&mut self, token: Token, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        match token.kind() {
            TokenKind::Let => {
                let Stmt::Expr { expr: let_in, .. } = self.parse_let(token.span().start(), source_map, true, interner)
                else { todo!() };

                let_in
            }

            TokenKind::Var => {
                let Stmt::Expr { expr: var_in, .. } = self.parse_var(token.span().start(), source_map, true, interner)
                else { todo!() };

                var_in
            }

            TokenKind::Const => {
                let Stmt::Expr { expr: const_in, .. } = self.parse_const(token.span().start(), source_map, true, interner)
                else { todo!() };

                const_in
            }

            TokenKind::Fn => {
                let Stmt::Expr { expr: fn_in, .. } = self.parse_fn(token.span().start(), source_map, true, interner)
                else { todo!() };

                fn_in
            }

            _ => todo!()
        }
    }

    fn parse_lit(&mut self, token: Token, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        match token.kind() {
            TokenKind::Int => {
                // Inline Integer
                if token.payload() & 0x8000_0000 == 0 {
                    Expr::Int {
                        value: AstInt::Small(token.payload() & 0x7FFF_FFFF),
                        span: token.span()
                    }
                // Payload stores base
                } else {
                    let base = token.payload() & 0x7FFF_FFFF;
                    let number = if base != 10 {
                        &token.get_lexeme(source_map).replace('_', "")[2..]
                    } else {
                        &token.get_lexeme(source_map).replace('_', "")
                    };

                    Expr::Int {
                        value: AstInt::Large(Integer::parse_radix(number, base as i32).unwrap().into()),
                        span: token.span()
                    }
                }
            }

            TokenKind::Real => {
                let mut reached_decimal = false;
                let mut denom_size = 1;
                let mut fraction = token
                    .get_lexeme(source_map)
                    .chars()
                    .filter(|&d| d != '_')
                    .fold(String::from("/1"), |mut acc, e| {
                        if e != '.' {
                            acc.insert(acc.len() - denom_size - 1, e);

                            if reached_decimal {
                                acc.push('0');
                                denom_size += 1;
                            }
                        } else {
                            reached_decimal = true;
                        }

                        acc
                    });

                if fraction.len() == 2 {
                    fraction.insert(0, '1');
                }

                let expr = Expr::Real {
                    value: Rational::parse(fraction).unwrap().into(),
                    span: token.span()
                };

                expr
            }

            TokenKind::Sci => {
                #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                enum ExpDir {
                    Pos,
                    Neg,
                }

                let mut reached_decimal = false;
                let exponent_direction;
                let mut denom_size = 1;
                let mut fraction = String::from("/1");
                let lexeme = token.get_lexeme(source_map);
                let mut sep = lexeme.find(['e', 'E']).unwrap();

                for i in 0..sep {
                    let ch = &lexeme[i..=i];

                    if ch != "." {
                        fraction.insert_str(fraction.len() - denom_size - 1, ch);

                        if reached_decimal {
                            fraction.push('0');
                            denom_size += 1;
                        }
                    } else {
                        reached_decimal = true;
                    }
                }

                if &lexeme[sep+1..=sep+1] == "-" {
                    exponent_direction = ExpDir::Neg;
                    sep += 1;
                } else if &lexeme[sep+1..=sep+1] == "+" {
                    exponent_direction = ExpDir::Pos;
                    sep += 1;
                } else {
                    exponent_direction = ExpDir::Pos;
                }

                // Exponent portion must fit in a usize
                let int = (&lexeme[sep+1..]).parse::<usize>().unwrap();
                if let ExpDir::Pos = exponent_direction {
                    fraction.insert_str(fraction.len() - denom_size - 1, &"0".repeat(int));
                } else {
                    fraction.push_str(&"0".repeat(int));
                }

                let expr = Expr::Real {
                    value: Rational::parse(fraction).unwrap().into(),
                    span: token.span()
                };

                expr
            }

            TokenKind::Imag => {
                let lexeme = token.get_lexeme(source_map);

                if lexeme == "i" {
                    let expr = Expr::Imag {
                        value: Rational::ONE.to_owned(),
                        span: token.span()
                    };

                    expr
                } else {
                    let lexeme = &lexeme[..lexeme.len()-1];

                    let mut reached_decimal = false;
                    let mut denom_size = 1;
                    let mut fraction = lexeme
                        .chars()
                        .filter(|&d| d != '_')
                        .fold(String::from("/1"), |mut acc, e| {
                            if e != '.' {
                                acc.insert(acc.len() - denom_size - 1, e);

                                if reached_decimal {
                                    acc.push('0');
                                    denom_size += 1;
                                }
                            } else {
                                reached_decimal = true;
                            }

                            acc
                        });

                    if fraction.len() == 2 {
                        fraction.insert(0, '1');
                    }

                    let expr = Expr::Imag {
                        value: Rational::parse(fraction).unwrap().into(),
                        span: token.span()
                    };

                    expr
                }
            }

            TokenKind::Ident => Expr::Ident(token.try_into().unwrap()),
            
            TokenKind::StringStart => {
                let span_start = token.span().start();
                let span_end;
                let prefix = StringPrefix::try_from_u32(token.payload());

                let src = source_map
                    .get_source(self.current().span().source_id())
                    .data();
                let mut parts = vec![];
                let mut cur_text = String::new();

                loop {
                    let token = self.current();
                    let slice = &src[token.span().range()];

                    match token.kind() {
                        TokenKind::StringSegment => {
                            cur_text.push_str(slice);

                            self.advance();
                        }

                        TokenKind::EscapeSeq => {
                            cur_text.push(match slice {
                                "\\0"  => '\0',
                                "\\\"" => '\"',
                                "\\\\" => '\\',
                                "\\n"  => '\n',
                                "\\r"  => '\r',
                                "\\t"  => '\t',
                                "\\b"  => '\x08',
                                "\\f"  => '\x0c',
                                "\\v"  => '\x0b',
                                _ => unreachable!()
                            });

                            self.advance();
                        }

                        TokenKind::InterpolateStart => {
                            if !cur_text.is_empty() {
                                parts.push(StringPart::Text(cur_text));
                                cur_text = String::new();
                            }
                            
                            self.advance();
                            parts.push(StringPart::Expr(self.parse_expr(source_map, interner, 0)));
                        }

                        TokenKind::InterpolateEnd => {
                            self.advance();
                        },
                        
                        TokenKind::StringEnd => {
                            if !cur_text.is_empty() {
                                parts.push(StringPart::Text(cur_text));
                            }

                            span_end = self.current().span().end();
                            self.advance();
                            break
                        }

                        TokenKind::Error(_) => todo!("parse error in string"),

                        _ => unreachable!()
                    }
                }

                if let Ok(StringPrefix::M | StringPrefix::Fm | StringPrefix::Rm) = prefix {
                    Expr::Latex(Box::new(Expr::String {
                        parts,
                        span: Span::new(span_start, span_end, self.current().span().source_id())
                    }))
                } else {
                    Expr::String {
                        parts,
                        span: Span::new(span_start, span_end, self.current().span().source_id())
                    }
                }
            }

            TokenKind::LBracket => {
                let span_start = token.span().start();

                if let Some(rb) = self.take(TokenKind::RBracket) {
                    Expr::Array {
                        rows: vec![],
                        span: Span::new(span_start, rb.span().end(), rb.span().source_id())
                    }
                } else {
                    let mut rows = vec![];
                    let mut row = vec![];

                    loop {
                        row.push(self.parse_expr(source_map, interner, 0));

                        if let Some(rb) = self.take(TokenKind::RBracket) {
                            rows.push(row);
                            
                            break Expr::Array {
                                rows,
                                span: Span::new(span_start, rb.span().end(), rb.span().source_id())
                            }
                        } else if self.accept(TokenKind::Comma) {
                            if let Some(rb) = self.take(TokenKind::RBracket) {
                                rows.push(row);

                                break Expr::Array {
                                    rows,
                                    span: Span::new(span_start, rb.span().end(), rb.span().source_id())
                                }
                            }
                        } else if self.accept(TokenKind::Semicolon) {
                            rows.push(row);
                            row = vec![];

                            if let Some(rb) = self.take(TokenKind::RBracket) {
                                break Expr::Array {
                                    rows,
                                    span: Span::new(span_start, rb.span().end(), rb.span().source_id())
                                }
                            }
                        } else {
                            todo!("expected comma");
                        }
                    }
                }
            }

            _ => todo!("unknown primary expression starting at: {:?}", self.current_kind())
        }
    }

    // fn parse_operations(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     let mut operation_items = vec![];
        
    //     let span_start = self.current().span();
    //     let mut span_end = span_start.start() + 1; 

    //     macro_rules! update_span_end {
    //         ( $result:expr ) => {
    //             {
    //                 let result = $result;
    //                 span_end = result.span().end();

    //                 result
    //             }
    //         };

    //         ( $result:expr, $next:expr ) => {
    //             {
    //                 let result = $result;
    //                 span_end = result.span().end();
    //                 $next;

    //                 result
    //             }
    //         }
    //     }

    //     while !self.current().is_terminating(source_map) {
    //         operation_items.push(match self.current_kind() {
    //             TokenKind::LBrace => OperationItem::Expr(Box::new(update_span_end!(self.parse_block(source_map, interner)))),
                
    //             _ if self.current().is_builtin_operator(interner) => OperationItem::Oper(update_span_end!(Oper::from_token_payload_unchecked(self.current()), self.advance())),

    //             TokenKind::Operator => OperationItem::Oper(update_span_end!(Oper::from_token_payload_unchecked(self.current()), self.advance())),
                
    //             TokenKind::Backtick => OperationItem::OpLit(update_span_end!(self.parse_operator_literal(source_map, interner))),

    //             _ => {
    //                 let expr = self.parse_call(source_map, interner);

    //                 if let Expr::Ident(name) = expr {
    //                     OperationItem::Ident(update_span_end!(name))
    //                 } else {
    //                     OperationItem::Expr(Box::new(update_span_end!(expr)))
    //                 }
    //             }
    //         })
    //     }

    //     if operation_items.len() == 1 {
    //         match operation_items.into_iter().next().unwrap() {
    //             OperationItem::Expr(expr) => *expr,
    //             OperationItem::Ident(name) => Expr::Ident(name),
    //             _ => todo!("expected expression")
    //         }
    //     } else {
    //         Expr::Operations {
    //             items: operation_items,
    //             span: Span::new(span_start.start(), span_end, span_start.source_id())
    //         }
    //     }
    // }

    fn parse_block(&mut self, token: Token, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
        let span_start = token.span().start();
        let mut stmts = vec![];
        let mut tail = None;

        self.alias_resolver.enter_scope();

        while self.current_kind() != TokenKind::RBrace {
            if self.starts_non_expr_stmt() {
                let stmt = self.parse_non_expr_stmt(source_map, interner);
                stmts.push(stmt);
            } else {
                let expr = self.parse_expr(source_map, interner, 0);

                if let Some(semi) = self.take(TokenKind::Semicolon) {
                    stmts.push(Stmt::Expr {
                        span: Span::new(expr.span().start(), semi.span().end(), semi.span().source_id()),
                        expr
                    });
                } else {
                    tail = Some(Box::new(expr));
                    break
                }
            }
        }

        self.alias_resolver.exit_scope();

        let span = Span::new(span_start, self.current().span().end(), self.current().span().source_id());
        self.expect(TokenKind::RBrace);
        Expr::Block {
            stmts,
            tail,
            span
        }
    }

    // fn parse_or(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     let lhs = self.parse_xor(source_map, interner);

    //     if self.accept(TokenKind::Or) {
    //         let rhs = Box::new(self.parse_or(source_map, interner));

    //         Expr::Or {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else {
    //         lhs
    //     }
    // }

    // fn parse_xor(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     let lhs = self.parse_and(source_map, interner);

    //     if self.accept(TokenKind::Xor) {
    //         let rhs = Box::new(self.parse_xor(source_map, interner));

    //         Expr::Xor {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else {
    //         lhs
    //     }
    // }

    // fn parse_and(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     let lhs = self.parse_not(source_map, interner);

    //     if self.accept(TokenKind::And) {
    //         let rhs = Box::new(self.parse_and(source_map, interner));

    //         Expr::And {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else {
    //         lhs
    //     }
    // }

    // fn parse_not(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     if self.accept(TokenKind::Not) {
    //         let expr = self.parse_not(source_map, interner);
            
    //         Expr::Not {
    //             span: Span::new(expr.span().start(), expr.span().end(), expr.span().source_id()),
    //             expr: Box::new(expr)
    //         }
    //     } else {
    //         self.parse_comparison(source_map, interner)
    //     }
    // }

    // fn parse_comparison(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     let lhs = self.parse_range(source_map, interner);

    //     macro_rules! parse_comparison {
    //         ($node_kind:ident) => {
    //             {
    //                 let rhs = self.parse_comparison(source_map, interner);
    //                 if let Some((lhsr, _)) = rhs.is_comparison_node() {
    //                     Expr::And {
    //                         span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //                         lhs: Box::new(Expr::$node_kind {
    //                             span: Span::new(lhs.span().start(), lhsr.span().end(), lhsr.span().source_id()),
    //                             lhs: Box::new(lhs),
    //                             rhs: lhsr.to_owned()
    //                         }),
    //                         rhs: Box::new(rhs)
    //                     }
    //                 } else if let Expr::And { lhs: ref lhsr, .. } = rhs {
    //                     if let Some((lhsr, _)) = lhsr.is_comparison_node() {
    //                         Expr::And {
    //                             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //                             lhs: Box::new(Expr::$node_kind {
    //                                 span: Span::new(lhs.span().start(), lhsr.span().end(), lhsr.span().source_id()),
    //                                 lhs: Box::new(lhs),
    //                                 rhs: lhsr.to_owned()
    //                             }),
    //                             rhs: Box::new(rhs)
    //                         }
    //                     } else {
    //                         unreachable!()
    //                     }
    //                 } else {
    //                     Expr::$node_kind {
    //                         span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //                         lhs: Box::new(lhs),
    //                         rhs: Box::new(rhs)
    //                     }
    //                 }
    //             }
    //         };
    //     }

    //     if self.accept_op(source_map, "==") {
    //         parse_comparison!(Eq)
    //     } else if self.accept_op(source_map, "!=") {
    //         parse_comparison!(NotEq)
    //     } else if self.accept_op(source_map, "<") {
    //         parse_comparison!(Less)
    //     } else if self.accept_op(source_map, ">") {
    //         parse_comparison!(Greater)
    //     } else if self.accept_op(source_map, "<=") {
    //         parse_comparison!(LessEq)
    //     } else if self.accept_op(source_map, ">=") {
    //         parse_comparison!(GreaterEq)
    //     } else if self.accept(TokenKind::SlashIn) {
    //         parse_comparison!(In)
    //     } else if self.accept(TokenKind::SlashNotIn) {
    //         let expr = parse_comparison!(In);
            
    //         Expr::Not {
    //             span: Span::new(expr.span().start(), expr.span().end(), expr.span().source_id()),
    //             expr: Box::new(expr),
    //         }
    //     } else {
    //         lhs
    //     }
    // }

    // fn parse_range(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     let id = |x: Expr, _range_span: Span| x;
    //     let pos = |x: Expr, range_span: Span| Expr::UnaryPlus {
    //         span: Span::new(range_span.end() - 1, x.span().end(), x.span().source_id()),
    //         expr: Box::new(x)
    //     };
    //     let neg = |x: Expr, range_span: Span| Expr::Neg {
    //         span: Span::new(range_span.end() - 1, x.span().end(), x.span().source_id()),
    //         expr: Box::new(x)
    //     }; 

    //     let range_span = self.current().span();
    //     match self.current_op(source_map) {
    //         Some("..") => self.finish_discrete_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, false),
    //             id,
    //             range_span,
    //             interner),
    //         Some("..+") => self.finish_discrete_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, false),
    //             pos,
    //             range_span,
    //             interner),
    //         Some("..-") => self.finish_discrete_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, false),
    //             neg,
    //             range_span,
    //             interner),
    //         Some("<..") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some("<..+") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some("<..-") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some("..<") => self.finish_discrete_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, true),
    //             id,
    //             range_span,
    //             interner),
    //         Some("..<+") => self.finish_discrete_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, true),
    //             pos,
    //             range_span,
    //             interner),
    //         Some("..<-") => self.finish_discrete_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, true),
    //             neg,
    //             range_span,
    //             interner),
    //         Some("<..<") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some("<..<+") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some("<..<-") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some(":") => self.finish_cont_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, false),
    //             id,
    //             range_span,
    //             interner),
    //         Some(":+") => self.finish_cont_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, false),
    //             pos,
    //             range_span,
    //             interner),
    //         Some(":-") => self.finish_cont_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, false),
    //             neg,
    //             range_span,
    //             interner),
    //         Some("<:") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some("<:+") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some("<:-") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some(":<") => self.finish_cont_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, true),
    //             id,
    //             range_span,
    //             interner),
    //         Some(":<+") => self.finish_cont_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, true),
    //             pos,
    //             range_span,
    //             interner),
    //         Some(":<-") => self.finish_cont_range(
    //             source_map, 
    //             Endpoint::Unspecified, 
    //             (false, true),
    //             neg,
    //             range_span,
    //             interner),
    //         Some("<:<") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some("<:<+") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some("<:<-") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some("::") => self.finish_range_step(
    //             source_map,
    //             Endpoint::Unspecified,
    //             Endpoint::Unspecified,
    //             range_span.start(),
    //             interner),
    //         Some("<::") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         Some(":<:") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //         _ => {
    //             let lhs = Box::new(self.parse_additive(source_map, interner));

    //             let range_span = self.current().span();
    //             match self.current_op(source_map) {
    //                 Some("..") => self.finish_discrete_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, false),
    //                     id,
    //                     range_span,
    //                     interner),
    //                 Some("..+") => self.finish_discrete_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, false),
    //                     pos,
    //                     range_span,
    //                     interner),
    //                 Some("..-") => self.finish_discrete_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, false),
    //                     neg,
    //                     range_span,
    //                     interner),
    //                 Some("<..") => self.finish_discrete_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, false),
    //                     id,
    //                     range_span,
    //                     interner),
    //                 Some("<..+") => self.finish_discrete_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, false),
    //                     pos,
    //                     range_span,
    //                     interner),
    //                 Some("<..-") => self.finish_discrete_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, false),
    //                     neg,
    //                     range_span,
    //                     interner),
    //                 Some("..<") => self.finish_discrete_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, true),
    //                     id,
    //                     range_span,
    //                     interner),
    //                 Some("..<+") => self.finish_discrete_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, true),
    //                     pos,
    //                     range_span,
    //                     interner),
    //                 Some("..<-") => self.finish_discrete_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, true),
    //                     neg,
    //                     range_span,
    //                     interner),
    //                 Some("<..<") => self.finish_discrete_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, true),
    //                     id,
    //                     range_span,
    //                     interner),
    //                 Some("<..<+") => self.finish_discrete_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, true),
    //                     pos,
    //                     range_span,
    //                     interner),
    //                 Some("<..<-") => self.finish_discrete_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, true),
    //                     neg,
    //                     range_span,
    //                     interner),
    //                 Some(":") => self.finish_cont_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, false),
    //                     id,
    //                     range_span,
    //                     interner),
    //                 Some(":+") => self.finish_cont_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, false),
    //                     pos,
    //                     range_span,
    //                     interner),
    //                 Some(":-") => self.finish_cont_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, false),
    //                     neg,
    //                     range_span,
    //                     interner),
    //                 Some("<:") => self.finish_cont_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, false),
    //                     id,
    //                     range_span,
    //                     interner),
    //                 Some("<:+") => self.finish_cont_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, false),
    //                     pos,
    //                     range_span,
    //                     interner),
    //                 Some("<:-") => self.finish_cont_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, false),
    //                     neg,
    //                     range_span,
    //                     interner),
    //                 Some(":<") => self.finish_cont_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, true),
    //                     id,
    //                     range_span,
    //                     interner),
    //                 Some(":<+") => self.finish_cont_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, true),
    //                     pos,
    //                     range_span,
    //                     interner),
    //                 Some(":<-") => self.finish_cont_range(
    //                     source_map, 
    //                     Endpoint::Inclusive(lhs), 
    //                     (false, true),
    //                     neg,
    //                     range_span,
    //                     interner),
    //                 Some("<:<") => self.finish_cont_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, true),
    //                     id,
    //                     range_span,
    //                     interner),
    //                 Some("<:<+") => self.finish_cont_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, true),
    //                     pos,
    //                     range_span,
    //                     interner),
    //                 Some("<:<-") => self.finish_cont_range(
    //                     source_map,
    //                     Endpoint::Exclusive(lhs),
    //                     (true, true),
    //                     neg,
    //                     range_span,
    //                     interner),
    //                 Some("::") => self.finish_range_step(source_map, Endpoint::Inclusive(lhs), Endpoint::Unspecified, range_span.start(), interner),
    //                 Some("<::") => self.finish_range_step(source_map, Endpoint::Inclusive(lhs), Endpoint::Unspecified, range_span.start(), interner),
    //                 Some(":<:") => todo!("cannot specify exclusivity for an unspecified endpoint"),
    //                 _ => *lhs
    //             }
    //         }
    //     }
    // }

    // fn finish_discrete_range<W: Fn(Expr, Span) -> Expr>(&mut self, source_map: &SourceMap, lhs: Endpoint, exclusivity: (bool, bool), wrap: W, range_span: Span, interner: &ResolvedInterner) -> Expr {
    //     self.advance();

    //     let span_start = match lhs {
    //         Endpoint::Unspecified => range_span.start(),
    //         Endpoint::Inclusive(ref lhs) |
    //         Endpoint::Exclusive(ref lhs) => lhs.span().start()
    //     };

    //     if self.current().is_terminating(source_map) {
    //         Expr::Range {
    //             span: Span::new(span_start, range_span.end(), range_span.source_id()),
    //             lhs,
    //             rhs: Endpoint::Unspecified,
    //             step: RangeStep::Discrete(Box::new(Expr::Int {
    //                 value: 1.into(),
    //                 span: SourceMap::synthetic_span()
    //             }))
    //         }
    //     } else {
    //         let (rhs_span, rhs) = if exclusivity.1 {
    //             let expr = self.parse_additive(source_map, interner);
    //             (expr.span(), Endpoint::Exclusive(Box::new(wrap(expr, range_span))))
    //         } else {
    //             let expr = self.parse_additive(source_map, interner);
    //             (expr.span(), Endpoint::Inclusive(Box::new(wrap(expr, range_span))))
    //         };

    //         Expr::Range {
    //             span: Span::new(span_start, rhs_span.end(), rhs_span.source_id()),
    //             lhs,
    //             rhs,
    //             step: RangeStep::Discrete(Box::new(Expr::Int {
    //                 value: 1.into(),
    //                 span: SourceMap::synthetic_span()
    //             }))
    //         }
    //     }
    // }

    // fn finish_cont_range<W: Fn(Expr, Span) -> Expr>(&mut self, source_map: &SourceMap, lhs: Endpoint, exclusivity: (bool, bool), wrap: W, range_span: Span, interner: &ResolvedInterner) -> Expr {
    //     self.advance();

    //     let span_start = match lhs {
    //         Endpoint::Unspecified => range_span.start(),
    //         Endpoint::Inclusive(ref lhs) |
    //         Endpoint::Exclusive(ref lhs) => lhs.span().start()
    //     };

    //     if self.current().is_terminating(source_map) {
    //         Expr::Range {
    //             span: Span::new(span_start, range_span.end(), range_span.source_id()),
    //             lhs,
    //             rhs: Endpoint::Unspecified,
    //             step: RangeStep::Continuous
    //         }
    //     } else if let Some(":") = self.current_op(source_map) {
    //         todo!("': :' is invalid")
    //     } else {
    //         let (rhs_span, rhs) = if exclusivity.1 {
    //             let expr = self.parse_additive(source_map, interner);
    //             (expr.span(), Endpoint::Exclusive(Box::new(wrap(expr, range_span))))
    //         } else {
    //             let expr = self.parse_additive(source_map, interner);
    //             (expr.span(), Endpoint::Inclusive(Box::new(wrap(expr, range_span))))
    //         };

    //         if self.accept_op(source_map, ":") {
    //             self.finish_range_step(source_map, lhs, rhs, span_start, interner)
    //         } else {
    //             Expr::Range {
    //                 span: Span::new(span_start, rhs_span.end(), rhs_span.source_id()),
    //                 lhs,
    //                 rhs,
    //                 step: RangeStep::Discrete(Box::new(Expr::Int {
    //                     value: 1.into(),
    //                     span: SourceMap::synthetic_span()
    //                 }))
    //             }
    //         }
    //     }
    // }

    // fn finish_range_step(&mut self, source_map: &SourceMap, lhs: Endpoint, rhs: Endpoint, span_start: usize, interner: &ResolvedInterner) -> Expr {        
    //     let expr = self.parse_additive(source_map, interner);
    //     let step_span = expr.span();
    //     let step = RangeStep::Discrete(Box::new(expr));

    //     Expr::Range {
    //         lhs,
    //         rhs,
    //         step,
    //         span: Span::new(span_start, step_span.end(), step_span.source_id())
    //     }
    // }

    // fn parse_additive(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     let lhs = self.parse_multiplicative(source_map, interner);

    //     if self.accept_op(source_map, "+") {
    //         let rhs = Box::new(self.parse_additive(source_map, interner));

    //         Expr::Plus {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else if self.accept_op(source_map, "-") {
    //         let rhs = Box::new(self.parse_additive(source_map, interner));

    //         Expr::Minus {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else if self.accept_op(source_map, "+-") {
    //         let rhs = Box::new(self.parse_additive(source_map, interner));

    //         Expr::PlusMinus {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else if self.accept_op(source_map, "-+") {
    //         let rhs = Box::new(self.parse_additive(source_map, interner));

    //         Expr::MinusPlus {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else {
    //         lhs
    //     }
    // }

    // fn parse_multiplicative(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     let lhs = self.parse_exponentative(source_map, interner);

    //     if self.accept_op(source_map, "*") {
    //         let rhs = Box::new(self.parse_multiplicative(source_map, interner));

    //         Expr::Times {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else if self.accept_op(source_map, "/") {
    //         let rhs = Box::new(self.parse_multiplicative(source_map, interner));

    //         Expr::Divide {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else if self.accept_op(source_map, "//") {
    //         let rhs = Box::new(self.parse_multiplicative(source_map, interner));

    //         Expr::IntDivide {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else if self.accept_op(source_map, "%") {
    //         let rhs = Box::new(self.parse_multiplicative(source_map, interner));

    //         Expr::Mod {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else {
    //         lhs
    //     }
    // }

    // fn parse_exponentative(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     let lhs = self.parse_custom_operator(source_map, interner);

    //     if self.accept_op(source_map, "^") {
    //         let rhs = Box::new(self.parse_exponentative(source_map, interner));

    //         Expr::Exp {
    //             span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //             lhs: Box::new(lhs),
    //             rhs
    //         }
    //     } else {
    //         lhs
    //     }
    // }

    // fn parse_custom_operator(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     macro_rules! parse_potentially_infix {
    //         ($lhs:expr) => {
    //             if let TokenKind::Ident = self.current_kind() {
    //                 let operator = self.current().to_owned();
    //                 self.advance();

    //                 let rhs = if let Some(unary) = self.parse_builtin_unary(source_map, interner) {
    //                     unary
    //                 } else {
    //                     self.parse_call(source_map, interner)
    //                 };

    //                 Expr::Infix {
    //                     span: Span::new($lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //                     lhs: Box::new($lhs),
    //                     operator: Operation::Ident(operator.try_into().unwrap()),
    //                     rhs: Box::new(rhs)
    //                 }
    //             } else if let TokenKind::Backtick = self.current_kind() {
    //                 let operator = self.parse_operator_literal(source_map, interner);                      
    //                 let rhs = if let Some(unary) = self.parse_builtin_unary(source_map, interner) {
    //                     unary
    //                 } else {
    //                     self.parse_call(source_map, interner)
    //                 };

    //                 Expr::Infix {
    //                     span: Span::new($lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //                     lhs: Box::new($lhs),
    //                     operator: Operation::OpLit(operator),
    //                     rhs: Box::new(rhs)
    //                 }
    //             } else if self.current().can_be_operator() && !self.current().is_builtin_operator(interner) {
    //                 let operator = self.current().to_owned();
    //                 self.advance();

    //                 let rhs = if let Some(unary) = self.parse_builtin_unary(source_map, interner) {
    //                     unary
    //                 } else {
    //                     self.parse_call(source_map, interner)
    //                 };

    //                 Expr::Infix {
    //                     span: Span::new($lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
    //                     lhs: Box::new($lhs),
    //                     operator: Operation::Oper(Oper::try_from(operator).unwrap()),
    //                     rhs: Box::new(rhs)
    //                 }
    //             } else {
    //                 $lhs
    //             }
    //         }
    //     }
        
    //     match self.current_kind() {
    //         // prefix operation
    //         TokenKind::Operator => {
    //             if let Some(unary) = self.parse_builtin_unary(source_map, interner) {
    //                 parse_potentially_infix!(unary)
    //             } else {
    //                 let operator = self.current().to_owned();
    //                 self.advance();

    //                 let operand = if let Some(unary) = self.parse_builtin_unary(source_map, interner) {
    //                     unary
    //                 } else {
    //                     self.parse_call(source_map, interner)
    //                 };

    //                 Expr::Prefix {
    //                     span: Span::new(operator.span().start(), operand.span().end(), operand.span().source_id()),
    //                     operator: Operation::Oper(Oper::try_from(operator).unwrap()),
    //                     operand: Box::new(operand)
    //                 }
    //             }
    //         }

    //         // ident as prefix operation
    //         TokenKind::Ident if 
    //             !matches!(self.peek_kind(), TokenKind::Operator |
    //                                         TokenKind::Dot      |
    //                                         TokenKind::Comma    |
    //                                         TokenKind::Semicolon|
    //                                         TokenKind::LParen   |   // `f (x)` is a function call.
    //                                         TokenKind::RParen   |   // To have it be an operation,
    //                                         TokenKind::LBracket |   // use `f {x}`.
    //                                         TokenKind::RBracket |
    //                                         TokenKind::RBrace   |
    //                                         TokenKind::Backtick |
    //                                         TokenKind::EOF)
    //             && !self.peek_kind().is_keyword() => {
    //             let operator = self.current().to_owned();
    //             self.advance();

    //             let operand = if let Some(unary) = self.parse_builtin_unary(source_map, interner) {
    //                 unary
    //             } else {
    //                 self.parse_call(source_map, interner)
    //             };

    //             Expr::Prefix {
    //                 span: Span::new(operator.span().start(), operand.span().end(), operand.span().source_id()),
    //                 operator: Operation::Ident(operator.try_into().unwrap()),
    //                 operand: Box::new(operand)
    //             }
    //         }

    //         // operator literal as prefix operation
    //         TokenKind::Backtick => {
    //             let span_start = self.current().span().start();
    //             let operator = self.parse_operator_literal(source_map, interner);
    //             let operand = if let Some(unary) = self.parse_builtin_unary(source_map, interner) {
    //                 unary
    //             } else {
    //                 self.parse_call(source_map, interner)
    //             };
                
    //             Expr::Prefix {
    //                 span: Span::new(span_start, operand.span().end(), operand.span().source_id()),
    //                 operator: Operation::OpLit(operator),
    //                 operand: Box::new(operand)
    //             }
    //         }

    //         // potential ident/operation as infix operation
    //         _ => {
    //             let lhs = if let Some(unary) = self.parse_builtin_unary(source_map, interner) {
    //                 unary
    //             } else {
    //                 self.parse_call(source_map, interner)
    //             };

    //             parse_potentially_infix!(lhs)
    //         }
    //     }
    // }

    /// Backtick must have been consumed before calling this function
    fn parse_operator_literal(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> OpLit {
        let Some(bt) = self.require(TokenKind::Backtick)
        else { todo!("expected backtick") };
        let span_start = bt.span().start();

        let assoc = if let Some(minvoke) = self.take(TokenKind::MacroInvoke) {
            match interner.resolve(&Spur::try_from_usize(minvoke.payload() as usize).unwrap()) {
                "lassoc" => Assoc::Left,
                "rassoc" => Assoc::Right,
                other => todo!("expected only @lassoc or @rassoc in associativity specifier, instead found {other}")
            }
        } else {
            Assoc::None
        };

        let name = if let Some(name) = self.require(TokenKind::Ident) {
            name.try_into().unwrap()
        } else {
            todo!("invalid operator literal; expected identifier")
        };

        let prec = if self.accept_op(source_map, ":") {
            let Some(token) = self.require(TokenKind::Int)
            else { todo!("expected natural for precedence level") };

            if token.payload() > Prec::MAX_CUSTOM_PREC as u32 {
                todo!("precedence level must be between 0 and {}", Prec::MAX_CUSTOM_PREC as u32);
            }

            Some(Prec::try_from(token.payload()).unwrap())
        } else {
            None
        };

        let Some(bt) = self.require(TokenKind::Backtick)
        else { todo!("expected backtick") };

        if let None = prec {
            OpLit::with_assoc(
                assoc,
                name,
                Span::new(span_start, bt.span().end(), bt.span().source_id()))
        } else {
            OpLit::new(
                assoc, 
                name, 
                prec.unwrap(), 
                Span::new(span_start, bt.span().end(), bt.span().source_id()))
        }
    }

    /// Attempts to parse a built-in unary expression. If it succeeds, it outputs the expression. If it cannot find a built-in unary operator, it returns None.
    // fn parse_builtin_unary(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Option<Expr> {
    //     if let Some(plus) = self.take_op(source_map, "+") {
    //         let expr = self.parse_call(source_map, interner);

    //         Some(Expr::UnaryPlus {
    //             span: Span::new(plus.span().start(), expr.span().end(), expr.span().source_id()),
    //             expr: Box::new(expr),
    //         })
    //     } else if let Some(neg) = self.take_op(source_map, "-") {
    //         let expr = self.parse_call(source_map, interner);

    //         Some(Expr::Neg {
    //             span: Span::new(neg.span().start(), expr.span().end(), expr.span().source_id()),
    //             expr: Box::new(expr)
    //         })
    //     } else if let Some(spread) = self.take_op(source_map, "...") {
    //         let expr = self.parse_call(source_map, interner);

    //         Some(Expr::Spread {
    //             span: Span::new(spread.span().start(), expr.span().end(), expr.span().source_id()),
    //             expr: Box::new(expr)
    //         })
    //     } else {
    //         None
    //     }
    // }

    /// Parses function calling, indexing, and dot access.
    // fn parse_call(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     let mut expr = self.parse_grouping(source_map, interner);

    //     loop {
    //         if self.accept(TokenKind::LParen) {
    //             expr = self.finish_call(source_map, expr, interner)
    //         } else if self.accept(TokenKind::LBracket) {
    //             expr = self.finish_index(source_map, expr, interner)
    //         } else if self.current().is_accessor(source_map) {
    //             expr = self.finish_access(source_map, expr)
    //         } else { // TODO:  dot access via a.b
    //                 // Both must be in this function because they must be same precedence as each other
    //             break expr
    //         }
    //     }
    // }

    // fn finish_call(&mut self, source_map: &SourceMap, callee: Expr, interner: &ResolvedInterner) -> Expr {
    //     let mut args = vec![];
    //     let mut kwargs = vec![];
    //     let mut in_kwargs = false;

    //     if let Some(rp) = self.take(TokenKind::RParen) {            
    //         return Expr::Call {
    //             span: Span::new(callee.span().start(), rp.span().end(), rp.span().source_id()),
    //             callee: Box::new(callee),
    //             args,
    //             kwargs,
    //         };
    //     }

    //     loop {
    //         if args.len() > Self::MAX_ARGS {
    //             todo!("too many arguments")
    //         }

    //         // kwarg
    //         if matches!(self.current_kind(), TokenKind::Ident) && matches!(self.peek_kind(), TokenKind::Eq) {
    //             in_kwargs = true;
                
    //             let arg = self.current().try_into().unwrap();
                
    //             self.advance();
    //             self.expect(TokenKind::Eq);

    //             let value = if let TokenKind::Comma | TokenKind::RParen = self.current_kind() {
    //                 Expr::Ident(arg)
    //             } else {
    //                 self.parse_expr(source_map, interner)
    //             };

    //             kwargs.push((arg, value));
    //         // args
    //         } else if in_kwargs {
    //             todo!("positional arguments cannot appear after keyword arguments")
    //         } else {
    //             args.push(self.parse_expr(source_map, interner))
    //         }

    //         if let Some(rp) = self.take(TokenKind::RParen) {                
    //             return Expr::Call {
    //                 span: Span::new(callee.span().start(), rp.span().end(), rp.span().source_id()),
    //                 callee: Box::new(callee),
    //                 args,
    //                 kwargs
    //             }
    //         } else if self.expect(TokenKind::Comma) {
    //             if let Some(rp) = self.take(TokenKind::RParen) {                    
    //                 return Expr::Call {
    //                     span: Span::new(callee.span().start(), rp.span().end(), rp.span().source_id()),
    //                     callee: Box::new(callee),
    //                     args,
    //                     kwargs
    //                 }
    //             }
    //         }
    //     }
    // }

    // fn finish_index(&mut self, source_map: &SourceMap, indexee: Expr, interner: &ResolvedInterner) -> Expr {
    //     let mut args = vec![];

    //     if self.accept(TokenKind::RBracket) {
    //         todo!("index operation must have at least one argument")
    //     }

    //     loop {
    //         if args.len() > Self::MAX_ARGS {
    //             todo!("too many arguments")
    //         }

    //         args.push(self.parse_expr(source_map, interner));

    //         if let Some(rb) = self.take(TokenKind::RBracket) {                
    //             return Expr::Index {
    //                 span: Span::new(indexee.span().start(), rb.span().end(), rb.span().source_id()),
    //                 indexee: Box::new(indexee),
    //                 args,
    //             }
    //         } else if self.expect(TokenKind::Comma) {
    //             if let Some(rb) = self.take(TokenKind::RBracket) {                    
    //                 return Expr::Index {
    //                     span: Span::new(indexee.span().start(), rb.span().end(), rb.span().source_id()),
    //                     indexee: Box::new(indexee),
    //                     args,
    //                 }
    //             }
    //         }
    //     }
    // }

    // fn finish_access(&mut self, source_map: &SourceMap, accessee: Expr) -> Expr {
    //     if self.accept(TokenKind::Dot) {
    //         if let Some(member) = self.take(TokenKind::Ident) {
    //             Expr::MemberAccess {
    //                 span: Span::new(accessee.span().start(), member.span().end(), member.span().source_id()),
    //                 accessee: Box::new(accessee),
    //                 member: member.try_into().unwrap()
    //             }
    //         } else {
    //             todo!("expected identifier")
    //         }
    //     } else if self.accept_op(source_map, ".@") {
    //         todo!("dot macro")
    //     } else {
    //         todo!()
    //     }
    // }

    // fn parse_grouping(&mut self, source_map: &SourceMap, interner: &ResolvedInterner) -> Expr {
    //     if let Some(lp) = self.take(TokenKind::LParen) {
    //         let span_start = lp.span().start();

    //         if let Some(rp) = self.take(TokenKind::RParen) {
    //             Expr::Unit {
    //                 span: Span::new(span_start, rp.span().end(), rp.span().source_id())
    //             }
    //         } else {
    //             let expr = self.parse_expr(source_map, interner);

    //             if let Some(rp) = self.take(TokenKind::RParen) {
    //                 Expr::Grouping {
    //                     expr: Box::new(expr),
    //                     span: Span::new(span_start, rp.span().end(), rp.span().source_id())
    //                 }
    //             } else if self.expect(TokenKind::Comma) {
    //                 let mut exprs = vec![expr];

    //                 if let Some(rp) = self.take(TokenKind::RParen) {
    //                     Expr::Tuple {
    //                         exprs,
    //                         span: Span::new(span_start, rp.span().end(), rp.span().source_id()) 
    //                     }
    //                 } else {
    //                     loop {
    //                         exprs.push(self.parse_expr(source_map, interner));

    //                         if let Some(rp) = self.take(TokenKind::RParen) {
    //                             break Expr::Tuple {
    //                                 exprs,
    //                                 span: Span::new(span_start, rp.span().end(), rp.span().source_id())
    //                             }
    //                         } else if self.expect(TokenKind::Comma) {
    //                             ()
    //                         } else {
    //                             todo!()
    //                         }
    //                     }
    //                 }
    //             } else {
    //                 todo!("expected ')'")
    //             }
    //         }
    //     } else {
    //         self.parse_def_in(source_map, interner)
    //     }
    // }

    // fn parse_cases(&mut self, source_map: &SourceMap, in_expr: bool, interner: &ResolvedInterner) -> Expr {        
    //     match self.current_kind() {
    //         TokenKind::If => {
    //             // let span_start = self.current().span().start();
    //             // self.advance();

    //             // let mut expr = self.parse_operations(source_map, interner);
    //             // match &mut expr {
    //             //     Expr::Operations { special_case, span, .. } => {
    //             //         *special_case = OperationsSpecialCase::If;
    //             //         span.set_start(span_start);
    //             //     }

    //             //     _ => ()
    //             // }

    //             todo!()

    //             // expr
    //         }

    //         // TokenKind::Match => {
    //         //     let span_start = self.current().span().start();
    //         //     self.advance();

    //         //     let mut expr = self.parse_operations(source_map, interner);
    //         // }

    //         _ => self.parse_primary(source_map, interner)
    //     }
    // }

    fn require_ident_and_resolve_alias(&mut self) -> Var {
        let Some(ident) = self.require(TokenKind::Ident) else { todo!("expected identifier") };
        let mut var = ident.try_into().unwrap();
        self.alias_resolver.resolve_var_to_var(&mut var);

        var
    }

    /// Checks if the current token matches the given `TokenKind`. If so, it advances to the next token and outputs `true`. Otherwise it stays put and outputs `false`.
    fn accept(&mut self, kind: TokenKind) -> bool {
        if kind == self.current_kind() {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Checks if the current token is an operator and matches the given operator lexeme. If so, it advances to the next token and outputs `true`. Otherwie it stays put and outputs `false`.
    fn accept_op(&mut self, source_map: &SourceMap, op: &str) -> bool {
        let Some(current_op) = self.current_op(source_map) else {
            return false
        };

        if op == current_op {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Checks if the current token matches the given `TokenKind`. If so, it advances to the next token and outputs the passed `Token`. If not, it outputs `None`.
    fn take(&mut self, kind: TokenKind) -> Option<Token> {
        if kind == self.current_kind() {
            let token = self.current();
            self.advance();
            
            Some(token)
        } else {
            None
        }
    }

    /// Checks if the current token matches the given `TokenKind`. If so, it advances to the next token and outputs the passed `Token`. If not, it outputs `None`.
    fn take_op(&mut self, source_map: &SourceMap, op: &str) -> Option<Token> {        
        if self.current_op(source_map)? == op {
            let token = self.current();
            self.advance();
            
            Some(token)
        } else {
            None
        }
    }

    /// Expects that the current token matches the given `TokenKind`. If so, it advances and outputs true. Otherwise it reports an error and outputs `false`.
    fn expect(&mut self, kind: TokenKind) -> bool {
        if self.accept(kind) {
            true
        } else {
            println!("expected {kind:?} found {:?}", self.current_kind());
            self.error(self.current());
            false
        }
    }

    /// Expects that the current token is an operator and that it matches the given operator lexeme. If so, it advances and outputs true. Otherwise is reports an error and outputs `false`.
    fn expect_op(&mut self, source_map: &SourceMap, op: &str) -> bool {
        if self.accept_op(source_map, op) {
            true
        } else {
            println!("expected {op:?} found {:?}", self.current().get_lexeme(source_map));
            self.error(self.current());
            false
        }
    }

    fn require(&mut self, kind: TokenKind) -> Option<Token> {
        if let Some(token) = self.take(kind) {
            Some(token)
        } else {
            println!("expected {kind:?} found {:?}", self.current_kind());
            self.error(self.current());
            None
        }
    }

    fn require_op(&mut self, source_map: &SourceMap, op: &str) -> Option<Token> {
        if let Some(token) = self.take_op(source_map, op) {
            Some(token)
        } else {
            println!("expected {op:?} found {:?}", self.current_kind());
            self.error(self.current());
            None
        }
    }

    fn error(&self, token: Token) {
        todo!("@ {:?}", token)
    }

    fn error_at(&self, token: &Token) {
        todo!("@ {:#?}", token)
    }

    /// Advances to the next token in the token stream and outputs the token just passed over. If it is at end, it will keep yielding EOF.
    fn advance(&mut self) -> Token {
        let current = self.current();
        self.current_token = self.tokens.next().unwrap();

        current
    }

    fn at_end(&self) -> bool {
        self.current_kind() == TokenKind::EOF
    }

    fn peek(&mut self) -> Token {
        *self.tokens.peek().unwrap()
    }

    fn peek_kind(&mut self) -> TokenKind {
        self.peek().kind()
    }

    fn current(&self) -> Token {
        self.current_token
    }

    fn current_kind(&self) -> TokenKind {
        self.current().kind()
    }

    fn current_op<'s>(&self, source_map: &'s SourceMap) -> Option<&'s str> {        
        if let TokenKind::Operator = self.current_kind() {
            let span = self.current().span();
            let source = source_map.get_source(span.source_id());
            
            Some(&source.data()[span.range()])
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum OperatorKey<'s> {
    Kind(TokenKind),
    Oper(&'s str)
}

#[derive(Debug, Clone, Copy)]
struct OperatorEntry<'r> {
    nud: Option<fn(&mut Parser<'r>, Token, &SourceMap, &ResolvedInterner) -> Expr>,
    nud_prec: u32,
    led: Option<fn(&mut Parser<'r>, Token, Expr, &SourceMap, &ResolvedInterner) -> Expr>,
    led_prec: u32
}

#[derive(Debug, Clone)]
enum EntryOrExpr<'r> {
    Entry(OperatorEntry<'r>),
    Expr(Expr)
}

// A struct to store the macro and alias definitions for each scope. This is only used until aliases and macros have been expanded.
// #[derive(Debug, Clone, PartialEq, Eq)]
// struct ExpEnv {
//     aliases: Vec<Alias>,
//     macros: Vec<Macro>,
//     parent: Option<Box<ExpEnv>>,
//     children: Vec<ExpEnv>
// }

// TODO: Change Recursive Descent Parser into Pratt Parser for expressions

/* Precedence Levels            Associativity
LOWEST
->                              N
or                              L
xor                             L
and                             L
not                             _
== != < > <= >= \in \notin      L
ranges                          _
+ - +- -+                       L
* / // % %%                     L
^                               R
@                               R
user (like `f`)                 N
unary                           N
index call access               L
{}                              N
()                              N
HIGHEST                 
*/

// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// enum ExprPrec {
//     Lambda,
//     Or,
//     Xor,
//     And,
//     Not,
//     Comparison, // note that it is parsed differently due to comparison chaining
//     Range,
//     Additive,
//     Multiplicative,
//     Exponentative,
//     User, // ident or non-builtin oper or oplit
//     Unary,
//     Index,
//     Call,
//     Dot,
//     Group
// }

// #[derive(Debug, Clone, Copy, PartialEq, Eq)]
// enum ExprAssoc {
//     Left,
//     Right, 
//     None
// }





// TODO: Error detection and synchronization

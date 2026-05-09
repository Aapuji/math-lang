use std::iter::{Chain, Peekable, Repeat, repeat};
use std::vec::IntoIter;

use rug::{Integer, Rational};

use crate::{ast::*, source};
use crate::source::{SourceMap, Span};
use crate::token::{Token, TokenKind};

type TokenStream = Peekable<Chain<IntoIter<Token>, Repeat<Token>>>;

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: TokenStream,
    current_token: Token
}

impl Parser {
    pub const MAX_ARGS: usize = 255;

    pub fn new(tokens: Vec<Token>) -> Self {
        let last = *tokens.last().unwrap();
        let mut t = tokens
            .into_iter()
            .chain(repeat(last))
            .peekable();
        let current = t.next().unwrap();

        Self {
            tokens: t,
            current_token: current,
        }
    }

    pub fn parse(mut self, source_map: &mut SourceMap) -> Vec<Stmt> {
        let mut stmts = vec![];
        // let top_env = ExpEnv::new();

        while !self.at_end() {
            if self.accept(TokenKind::Semicolon) {
                continue
            }            

            stmts.push(self.parse_stmt(source_map));
        }

        stmts
    }

    fn parse_stmt(&mut self, source_map: &SourceMap) -> Stmt {
        if self.starts_non_expr_stmt() {
            self.parse_non_expr_stmt(source_map)
        } else {
            self.parse_expr_stmt(source_map)
        }
    }

    fn starts_non_expr_stmt(&self) -> bool {
        matches!(self.current_kind(), 
              TokenKind::Let
            | TokenKind::Var
            | TokenKind::Const
            | TokenKind::Fn
            | TokenKind::Sym
            | TokenKind::Enum
            | TokenKind::Struct
            | TokenKind::Alias
            | TokenKind::Using
        )
    }

    fn parse_non_expr_stmt(&mut self, source_map: &SourceMap) -> Stmt {
        match self.current_kind() {
            TokenKind::Let => self.parse_let(source_map, false),
            TokenKind::Var => self.parse_var(source_map, false),
            TokenKind::Const => self.parse_const(source_map, false),
            TokenKind::Fn => self.parse_fn(source_map, false),
            TokenKind::Sym => self.parse_sym(source_map),
            TokenKind::Enum => self.parse_enum(source_map), // TODO: determine if we should have `in` for enum & struct
            TokenKind::Struct => self.parse_struct(source_map),
            _ => todo!()
        }
    }

    fn parse_let(&mut self, source_map: &SourceMap, in_expr: bool) -> Stmt {
        let span_start = self.current().span().start();
        self.accept(TokenKind::Let);

        let bindings = self.parse_bindings(source_map);
        let (kind, value) = if self.accept(TokenKind::Eq) {
            (LetKind::Assign, Some(self.parse_expr(source_map)))
        } else if self.accept(TokenKind::ColonEq) {
            (LetKind::Define, Some(self.parse_expr(source_map)))
        } else {
            (LetKind::Declare, None)
        };

        if self.accept(TokenKind::In) {
            let expr = self.parse_expr(source_map);

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

    fn parse_bindings(&mut self, source_map: &SourceMap) -> Vec<Binding> {
        let mut bindings = vec![];

        loop {
            bindings.push(self.parse_binding(source_map));

            if self.accept(TokenKind::Comma) {
                continue
            } else {
                break
            }
        }

        bindings
    }

    // TODO: Record, Tuple, Destructuring, Rest, and _ bindings, and distinguishing Tuple Constructor from Function Call
    fn parse_binding(&mut self, source_map: &SourceMap) -> Binding {
        let name: Var = self.require(TokenKind::Ident)
            .unwrap_or_else(|| todo!("identifier expected"))
            .try_into()
            .unwrap();
        let span_start = name.span().start();

        if let Some("<") = self.current_op(source_map) {
            Binding::Fn(self.finish_header(source_map, name, span_start))
        } else if let TokenKind::LParen = self.current_kind() {
            Binding::Call(self.finish_header(source_map, name, span_start))
        } else {
            Binding::Name(name, self.parse_type_annotation(source_map))
        }
    }

    fn parse_var(&mut self, source_map: &SourceMap, in_expr: bool) -> Stmt {        
        let span_start = self.current().span().start();
        self.accept(TokenKind::Var);
        
        let TokenKind::Ident = self.current_kind()
        else { todo!("expected identifier") };

        let name = self.current().try_into().unwrap();
        self.advance();

        let ty = self.parse_type_annotation(source_map);
        let def = if self.accept(TokenKind::Eq) {
            Some(self.parse_expr(source_map))
        } else { None };

        if self.accept(TokenKind::In) {
            let expr = self.parse_expr(source_map);

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

    fn parse_const(&mut self, source_map: &SourceMap, in_expr: bool) -> Stmt {        
        let span_start = self.current().span().start();
        self.accept(TokenKind::Const);
        
        let TokenKind::Ident = self.current_kind()
        else { todo!("expected identifier") };

        let name = self.current().try_into().unwrap();
        self.advance();

        let ty = self.parse_type_annotation(source_map);
        let def = if self.accept(TokenKind::Eq) {
            Some(self.parse_expr(source_map))
        } else { None };

        if self.accept(TokenKind::In) {
            let expr = self.parse_expr(source_map);

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

    fn parse_fn(&mut self, source_map: &SourceMap, in_expr: bool) -> Stmt {
        let span_start = self.current().span().start();
        self.accept(TokenKind::Fn);

        let header = self.parse_header(source_map);
        let (span_end, value) = if self.accept(TokenKind::Eq) {
            let expr = self.parse_expr(source_map);

            let span_end = if in_expr {
                expr.span().end()
            } else {
                let span_end = self.current().span().end();
                self.expect(TokenKind::Semicolon);

                span_end
            };

            (span_end, expr)
        } else if let TokenKind::LBrace = self.current_kind() {
            let block = self.parse_block(source_map);

            (block.span().end(), block)
        } else {
            todo!("expected block for fn def");
        };

        if self.accept(TokenKind::In) {
            let expr = self.parse_expr(source_map);

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

    fn parse_header(&mut self, source_map: &SourceMap) -> FnHeader {
        let name: Var = self.require(TokenKind::Ident)
            .unwrap_or_else(|| todo!("identifier expected"))
            .try_into()
            .unwrap();
        let span_start = name.span().start();

        self.finish_header(source_map, name, span_start)
    }

    fn finish_header(&mut self, source_map: &SourceMap, name: Var, span_start: usize) -> FnHeader {
        let ty_args = self.parse_generic(source_map);
        let (args, kwargs, args_span_end) = self.parse_args_def(source_map);
        let ty = self.parse_type_annotation(source_map);

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
    fn parse_args_def(&mut self, source_map: &SourceMap) -> (Vec<(Var, Option<Type>)>, Vec<(Var, Option<Type>)>, usize) {
        let mut args = vec![];
        let mut kwargs = vec![];
        let mut in_kwargs = false;

        self.expect(TokenKind::LParen);
        
        if let Some(rp) = self.take(TokenKind::RParen) {
            return (args, kwargs, rp.span().end());
        }

        loop {
            let Some(arg) = self.take(TokenKind::Ident) 
            else { todo!("expected identifier") };

            let arg = arg.try_into().unwrap();
            let ty = self.parse_type_annotation(source_map);
            
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
    fn parse_type_annotation(&mut self, source_map: &SourceMap) -> Option<Type> {
        // : Type
        if self.accept_op(source_map, ":") {
            Some(self.parse_type(source_map))
        // :: Implicit Refinement
        } else if self.accept_op(source_map, "::") {
            todo!()
        } else {
            None
        }
    }

    fn parse_sym(&mut self, source_map: &SourceMap) -> Stmt {
        let span_start = self.current().span().start();
        self.accept(TokenKind::Sym);

        let Some(name) = self.require(TokenKind::Ident)
        else { todo!("expected identifier") };
        let name = name.try_into().unwrap();

        let mut args = if let TokenKind::LParen = self.current_kind() {
            let (args, kwargs, _) = self.parse_args_def(source_map);
            if !kwargs.is_empty() {
                todo!("keyword arguments are not allowed in a symbolic node definition");
            }

            args
        } else {
            vec![]
        };

        let ty = self.parse_type_annotation(source_map);

        let Some(semi) = self.require(TokenKind::Semicolon)
        else { todo!("expected semicolon") };

        Stmt::Sym {
            name,
            args,
            ty,
            span: Span::new(span_start, semi.span().end(), semi.span().source_id())
        }
    }

    fn parse_enum(&mut self, source_map: &SourceMap) -> Stmt {
        let span_start = self.current().span().start();
        self.accept(TokenKind::Enum);

        let Some(name) = self.require(TokenKind::Ident)
        else { todo!("expected ident") };
        let name = name.try_into().unwrap();

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
            let Some(tag) = self.require(TokenKind::Ident)
            else { todo!("expected ident") };
            let tag: Var = tag.try_into().unwrap();

            if self.accept(TokenKind::LParen) {
                if self.accept(TokenKind::RParen) {
                    todo!("empty enum tuple variants are not allowed")
                }

                let mut data = vec![];
                loop {
                    data.push(self.parse_type(source_map));

                    if self.accept(TokenKind::RParen)
                        || (self.expect(TokenKind::Comma) && self.accept(TokenKind::RBrace)) {
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
                    let Some(key) = self.require(TokenKind::Ident)
                    else { todo!("expected ident") };
                    let key = key.try_into().unwrap();

                    self.expect_op(source_map, ":");
                    let ty = self.parse_type(source_map);

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

    fn parse_struct(&mut self, source_map: &SourceMap) -> Stmt {
        let span_start = self.current().span().start();
        self.accept(TokenKind::Struct);

        let Some(name) = self.require(TokenKind::Ident)
        else { todo!("expected ident") };
        let name = name.try_into().unwrap();

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
            let Some(field) = self.require(TokenKind::Ident)
            else { todo!("expected ident") };
            let field = field.try_into().unwrap();

            self.expect_op(source_map, ":");
            let ty = self.parse_type(source_map);

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
                let name = Var::try_from(name).unwrap();

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

    // TODO: this
    // fn parse_sat(&mut self, source_map: &SourceMap) -> Token {
    //     self.require(TokenKind::Ident).unwrap()
    // }

    fn parse_type(&mut self, source_map: &SourceMap) -> Type {        
        match self.current_kind() {
            TokenKind::LBracket => self.parse_array_type(source_map),
            TokenKind::LParen => self.parse_grouping_type(source_map),
            _ => self.parse_primary_type(source_map)
        }
    }

    fn parse_array_type(&mut self, source_map: &SourceMap) -> Type {
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
                    let expr = self.parse_primary(source_map);

                    match expr {
                        Expr::Ident(_)   |
                        Expr::Int { .. } => (),
                        _ => todo!("array shape indicactor can only hold an unknown qualifier ('?'), a whole number, or an identifier")
                    }

                    shape_specs.push(ShapeSpec::Known(expr));
                    finish_parsing_item!()
                }
            }

            Shape::Specified(shape_specs)
        };

        let ty = self.parse_type(source_map);

        Type::Array {
            span: Span::new(span_start, ty.span().end(), ty.span().source_id()),
            shape,
            ty: Box::new(ty)
        }
    }

    fn parse_grouping_type(&mut self, source_map: &SourceMap) -> Type {
        let span_start = self.current().span().start();
        self.accept(TokenKind::LParen);

        if let Some(rp) = self.take(TokenKind::RParen) {
            Type::Unit {
                span: Span::new(span_start, rp.span().end(), rp.span().source_id())
            }
        } else {
            let mut ty = self.parse_type(source_map);

            if let Some(rp) = self.take(TokenKind::RParen) {
                ty.span_mut().set_start(span_start);
                ty.span_mut().set_end(rp.span().end());
                ty
            } else {
                self.expect(TokenKind::Comma);
                if let Some(rp) = self.take(TokenKind::RParen) {
                    return Type::Tuple {
                        types: vec![ty],
                        span: Span::new(span_start, rp.span().end(), rp.span().source_id())
                    }
                }

                let mut types = vec![ty];

                loop {
                    let ty = self.parse_type(source_map);

                    // TODO: type exponent

                    types.push(ty);

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

    fn parse_primary_type(&mut self, source_map: &SourceMap) -> Type {
        match self.current_kind() {
            TokenKind::Ident => {
                let ty = Type::Named(self.current().try_into().unwrap());
                self.advance();
                
                ty
            }

            _ => todo!()
        }
    }

    fn parse_expr_stmt(&mut self, source_map: &SourceMap) -> Stmt {
        let expr = self.parse_expr(source_map);

        if let Some(semi) = self.take(TokenKind::Semicolon) {
            Stmt::Expr {
                span: Span::new(expr.span().start(), semi.span().end(), semi.span().source_id()),
                expr
            }
        } else {
            todo!("report error - expected semicolon")
        }
    }

    fn parse_expr(&mut self, source_map: &SourceMap) -> Expr {
        if let TokenKind::LBrace = self.peek_kind() {
            self.parse_block(source_map)
        } else {
            self.parse_or(source_map)
        }
    }

    fn parse_block(&mut self, source_map: &SourceMap) -> Expr {
        let span_start = self.current().span().start();
        self.accept(TokenKind::LBrace);
        
        let mut stmts = vec![];
        let mut tail = None;

        while self.current_kind() != TokenKind::RBrace {
            if self.starts_non_expr_stmt() {
                let stmt = self.parse_non_expr_stmt(source_map);
                stmts.push(stmt);
            } else {
                let expr = self.parse_expr(source_map);

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

        let span = Span::new(span_start, self.current().span().end(), self.current().span().source_id());
        self.expect(TokenKind::RBrace);
        Expr::Block {
            stmts,
            tail,
            span
        }
    }

    fn parse_or(&mut self, source_map: &SourceMap) -> Expr {
        let lhs = self.parse_xor(source_map);

        if self.accept(TokenKind::Or) {
            let rhs = Box::new(self.parse_or(source_map));

            Expr::Or {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else {
            lhs
        }
    }

    fn parse_xor(&mut self, source_map: &SourceMap) -> Expr {
        let lhs = self.parse_and(source_map);

        if self.accept(TokenKind::Xor) {
            let rhs = Box::new(self.parse_xor(source_map));

            Expr::Xor {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else {
            lhs
        }
    }

    fn parse_and(&mut self, source_map: &SourceMap) -> Expr {
        let lhs = self.parse_not(source_map);

        if self.accept(TokenKind::And) {
            let rhs = Box::new(self.parse_and(source_map));

            Expr::And {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else {
            lhs
        }
    }

    fn parse_not(&mut self, source_map: &SourceMap) -> Expr {
        if self.accept(TokenKind::Not) {
            let expr = self.parse_not(source_map);
            
            Expr::Not {
                span: Span::new(expr.span().start(), expr.span().end(), expr.span().source_id()),
                expr: Box::new(expr)
            }
        } else {
            self.parse_comparison(source_map)
        }
    }

    fn parse_comparison(&mut self, source_map: &SourceMap) -> Expr {
        let lhs = self.parse_range(source_map);

        macro_rules! parse_comparison {
            ($node_kind:ident) => {
                {
                    let rhs = self.parse_comparison(source_map);
                    if let Some((lhsr, _)) = rhs.is_comparison_node() {
                        Expr::And {
                            span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                            lhs: Box::new(Expr::$node_kind {
                                span: Span::new(lhs.span().start(), lhsr.span().end(), lhsr.span().source_id()),
                                lhs: Box::new(lhs),
                                rhs: lhsr.to_owned()
                            }),
                            rhs: Box::new(rhs)
                        }
                    } else if let Expr::And { lhs: ref lhsr, .. } = rhs {
                        if let Some((lhsr, _)) = lhsr.is_comparison_node() {
                            Expr::And {
                                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                                lhs: Box::new(Expr::$node_kind {
                                    span: Span::new(lhs.span().start(), lhsr.span().end(), lhsr.span().source_id()),
                                    lhs: Box::new(lhs),
                                    rhs: lhsr.to_owned()
                                }),
                                rhs: Box::new(rhs)
                            }
                        } else {
                            unreachable!()
                        }
                    } else {
                        Expr::$node_kind {
                            span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs)
                        }
                    }
                }
            };
        }

        if self.accept_op(source_map, "==") {
            parse_comparison!(Eq)
        } else if self.accept_op(source_map, "!=") {
            parse_comparison!(NotEq)
        } else if self.accept_op(source_map, "<") {
            parse_comparison!(Less)
        } else if self.accept_op(source_map, ">") {
            parse_comparison!(Greater)
        } else if self.accept_op(source_map, "<=") {
            parse_comparison!(LessEq)
        } else if self.accept_op(source_map, ">=") {
            parse_comparison!(GreaterEq)
        } else if self.accept(TokenKind::SlashIn) {
            parse_comparison!(In)
        } else if self.accept(TokenKind::SlashNotIn) {
            let expr = parse_comparison!(In);
            
            Expr::Not {
                span: Span::new(expr.span().start(), expr.span().end(), expr.span().source_id()),
                expr: Box::new(expr),
            }
        } else {
            lhs
        }
    }

    fn parse_range(&mut self, source_map: &SourceMap) -> Expr {
        let id = |x: Expr, _range_span: Span| x;
        let pos = |x: Expr, range_span: Span| Expr::UnaryPlus {
            span: Span::new(range_span.end() - 1, x.span().end(), x.span().source_id()),
            expr: Box::new(x)
        };
        let neg = |x: Expr, range_span: Span| Expr::Neg {
            span: Span::new(range_span.end() - 1, x.span().end(), x.span().source_id()),
            expr: Box::new(x)
        }; 

        let range_span = self.current().span();
        match self.current_op(source_map) {
            Some("..") => self.finish_discrete_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, false),
                id,
                range_span),
            Some("..+") => self.finish_discrete_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, false),
                pos,
                range_span),
            Some("..-") => self.finish_discrete_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, false),
                neg,
                range_span),
            Some("<..") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some("<..+") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some("<..-") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some("..<") => self.finish_discrete_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, true),
                id,
                range_span),
            Some("..<+") => self.finish_discrete_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, true),
                pos,
range_span),
            Some("..<-") => self.finish_discrete_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, true),
                neg,
                    range_span),
            Some("<..<") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some("<..<+") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some("<..<-") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some(":") => self.finish_cont_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, false),
                id,
                range_span),
            Some(":+") => self.finish_cont_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, false),
                pos,
                range_span),
            Some(":-") => self.finish_cont_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, false),
                neg,
                range_span),
            Some("<:") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some("<:+") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some("<:-") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some(":<") => self.finish_cont_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, true),
                id,
                range_span),
            Some(":<+") => self.finish_cont_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, true),
                pos,
                range_span),
            Some(":<-") => self.finish_cont_range(
                source_map, 
                Endpoint::Unspecified, 
                (false, true),
                neg,
                range_span),
            Some("<:<") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some("<:<+") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some("<:<-") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some("::") => self.finish_range_step(source_map, Endpoint::Unspecified, Endpoint::Unspecified, range_span.start()),
            Some("<::") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            Some(":<:") => todo!("cannot specify exclusivity for an unspecified endpoint"),
            _ => {
                let lhs = Box::new(self.parse_additive(source_map));

                let range_span = self.current().span();
                match self.current_op(source_map) {
                    Some("..") => self.finish_discrete_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, false),
                        id,
                        range_span),
                    Some("..+") => self.finish_discrete_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, false),
                        pos,
                    range_span),
                    Some("..-") => self.finish_discrete_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, false),
                        neg,
                        range_span),
                    Some("<..") => self.finish_discrete_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, false),
                        id,
                        range_span),
                    Some("<..+") => self.finish_discrete_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, false),
                        pos,
                        range_span),
                    Some("<..-") => self.finish_discrete_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, false),
                        neg,
                        range_span),
                    Some("..<") => self.finish_discrete_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, true),
                        id,
                        range_span),
                    Some("..<+") => self.finish_discrete_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, true),
                        pos,
                        range_span),
                    Some("..<-") => self.finish_discrete_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, true),
                        neg,
                        range_span),
                    Some("<..<") => self.finish_discrete_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, true),
                        id,
                        range_span),
                    Some("<..<+") => self.finish_discrete_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, true),
                        pos,
                        range_span),
                    Some("<..<-") => self.finish_discrete_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, true),
                        neg,
                        range_span),
                    Some(":") => self.finish_cont_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, false),
                        id,
                        range_span),
                    Some(":+") => self.finish_cont_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, false),
                        pos,
                        range_span),
                    Some(":-") => self.finish_cont_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, false),
                        neg,
                        range_span),
                    Some("<:") => self.finish_cont_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, false),
                        id,
                        range_span),
                    Some("<:+") => self.finish_cont_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, false),
                        pos,
                        range_span),
                    Some("<:-") => self.finish_cont_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, false),
                        neg,
                        range_span),
                    Some(":<") => self.finish_cont_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, true),
                        id,
                        range_span),
                    Some(":<+") => self.finish_cont_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, true),
                        pos,
                        range_span),
                    Some(":<-") => self.finish_cont_range(
                        source_map, 
                        Endpoint::Inclusive(lhs), 
                        (false, true),
                        neg,
                        range_span),
                    Some("<:<") => self.finish_cont_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, true),
                        id,
                        range_span),
                    Some("<:<+") => self.finish_cont_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, true),
                        pos,
                        range_span),
                    Some("<:<-") => self.finish_cont_range(
                        source_map,
                        Endpoint::Exclusive(lhs),
                        (true, true),
                        neg,
                        range_span),
                    Some("::") => self.finish_range_step(source_map, Endpoint::Inclusive(lhs), Endpoint::Unspecified, range_span.start()),
                    Some("<::") => self.finish_range_step(source_map, Endpoint::Inclusive(lhs), Endpoint::Unspecified, range_span.start()),
                    Some(":<:") => todo!("cannot specify exclusivity for an unspecified endpoint"),
                    _ => *lhs
                }
            }
        }
    }

    fn finish_discrete_range<W: Fn(Expr, Span) -> Expr>(&mut self, source_map: &SourceMap, lhs: Endpoint, exclusivity: (bool, bool), wrap: W, range_span: Span) -> Expr {
        self.advance();

        let span_start = match lhs {
            Endpoint::Unspecified => range_span.start(),
            Endpoint::Inclusive(ref lhs) |
            Endpoint::Exclusive(ref lhs) => lhs.span().start()
        };

        if self.current().is_terminating(source_map) {
            Expr::Range {
                span: Span::new(span_start, range_span.end(), range_span.source_id()),
                lhs,
                rhs: Endpoint::Unspecified,
                step: RangeStep::Discrete(Box::new(Expr::Int {
                    value: 1.into(),
                    span: SourceMap::synthetic_span()
                }))
            }
        } else {
            let (rhs_span, rhs) = if exclusivity.1 {
                let expr = self.parse_additive(source_map);
                (expr.span(), Endpoint::Exclusive(Box::new(wrap(expr, range_span))))
            } else {
                let expr = self.parse_additive(source_map);
                (expr.span(), Endpoint::Inclusive(Box::new(wrap(expr, range_span))))
            };

            Expr::Range {
                span: Span::new(span_start, rhs_span.end(), rhs_span.source_id()),
                lhs,
                rhs,
                step: RangeStep::Discrete(Box::new(Expr::Int {
                    value: 1.into(),
                    span: SourceMap::synthetic_span()
                }))
            }
        }
    }

    fn finish_cont_range<W: Fn(Expr, Span) -> Expr>(&mut self, source_map: &SourceMap, lhs: Endpoint, exclusivity: (bool, bool), wrap: W, range_span: Span) -> Expr {
        self.advance();

        let span_start = match lhs {
            Endpoint::Unspecified => range_span.start(),
            Endpoint::Inclusive(ref lhs) |
            Endpoint::Exclusive(ref lhs) => lhs.span().start()
        };

        if self.current().is_terminating(source_map) {
            Expr::Range {
                span: Span::new(span_start, range_span.end(), range_span.source_id()),
                lhs,
                rhs: Endpoint::Unspecified,
                step: RangeStep::Continuous
            }
        } else if let Some(":") = self.current_op(source_map) {
            todo!("': :' is invalid")
        } else {
            let (rhs_span, rhs) = if exclusivity.1 {
                let expr = self.parse_additive(source_map);
                (expr.span(), Endpoint::Exclusive(Box::new(wrap(expr, range_span))))
            } else {
                let expr = self.parse_additive(source_map);
                (expr.span(), Endpoint::Inclusive(Box::new(wrap(expr, range_span))))
            };

            if self.accept_op(source_map, ":") {
                self.finish_range_step(source_map, lhs, rhs, span_start)
            } else {
                Expr::Range {
                    span: Span::new(span_start, rhs_span.end(), rhs_span.source_id()),
                    lhs,
                    rhs,
                    step: RangeStep::Discrete(Box::new(Expr::Int {
                        value: 1.into(),
                        span: SourceMap::synthetic_span()
                    }))
                }
            }
        }
    }

    fn finish_range_step(&mut self, source_map: &SourceMap, lhs: Endpoint, rhs: Endpoint, span_start: usize) -> Expr {        
        let expr = self.parse_additive(source_map);
        let step_span = expr.span();
        let step = RangeStep::Discrete(Box::new(expr));

        Expr::Range {
            lhs,
            rhs,
            step,
            span: Span::new(span_start, step_span.end(), step_span.source_id())
        }
    }

    fn parse_additive(&mut self, source_map: &SourceMap) -> Expr {
        let lhs = self.parse_multiplicative(source_map);

        if self.accept_op(source_map, "+") {
            let rhs = Box::new(self.parse_additive(source_map));

            Expr::Plus {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "-") {
            let rhs = Box::new(self.parse_additive(source_map));

            Expr::Minus {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "+-") {
            let rhs = Box::new(self.parse_additive(source_map));

            Expr::PlusMinus {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "-+") {
            let rhs = Box::new(self.parse_additive(source_map));

            Expr::MinusPlus {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else {
            lhs
        }
    }

    fn parse_multiplicative(&mut self, source_map: &SourceMap) -> Expr {
        let lhs = self.parse_exponentative(source_map);

        if self.accept_op(source_map, "*") {
            let rhs = Box::new(self.parse_multiplicative(source_map));

            Expr::Times {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "/") {
            let rhs = Box::new(self.parse_multiplicative(source_map));

            Expr::Divide {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "//") {
            let rhs = Box::new(self.parse_multiplicative(source_map));

            Expr::IntDivide {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "%") {
            let rhs = Box::new(self.parse_multiplicative(source_map));

            Expr::Mod {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "%%") {
            let rhs = Box::new(self.parse_multiplicative(source_map));

            Expr::ModClass {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else {
            lhs
        }
    }

    fn parse_exponentative(&mut self, source_map: &SourceMap) -> Expr {
        let lhs = self.parse_custom_operator(source_map);

        if self.accept_op(source_map, "^") {
            let rhs = Box::new(self.parse_exponentative(source_map));

            Expr::Exp {
                span: Span::new(lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                lhs: Box::new(lhs),
                rhs
            }
        } else {
            lhs
        }
    }

    fn parse_custom_operator(&mut self, source_map: &SourceMap) -> Expr {
        macro_rules! parse_potentially_infix {
            ($lhs:expr) => {
                if let TokenKind::Ident = self.current_kind() {
                    let operator = self.current().to_owned();
                    self.advance();

                    let rhs = if let Some(unary) = self.parse_builtin_unary(source_map) {
                        unary
                    } else {
                        self.parse_call(source_map)
                    };

                    Expr::Infix {
                        span: Span::new($lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                        lhs: Box::new($lhs),
                        operator: Operation::Ident(operator.try_into().unwrap()),
                        rhs: Box::new(rhs)
                    }
                } else if self.accept(TokenKind::Backtick) {
                    if let Ok(name) = self.parse_operator_literal() {                        
                        let rhs = if let Some(unary) = self.parse_builtin_unary(source_map) {
                            unary
                        } else {
                            self.parse_call(source_map)
                        };

                        Expr::Infix {
                            span: Span::new($lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                            lhs: Box::new($lhs),
                            operator: Operation::OpLit(name),
                            rhs: Box::new(rhs)
                        }
                    } else {
                        todo!("after error with backtick")
                    }
                } else if self.current().can_be_operator() && !self.current().is_builtin_operator(source_map) {
                    let operator = self.current().to_owned();
                    self.advance();

                    let rhs = if let Some(unary) = self.parse_builtin_unary(source_map) {
                        unary
                    } else {
                        self.parse_call(source_map)
                    };

                    Expr::Infix {
                        span: Span::new($lhs.span().start(), rhs.span().end(), rhs.span().source_id()),
                        lhs: Box::new($lhs),
                        operator: Operation::Custom(operator),
                        rhs: Box::new(rhs)
                    }
                } else {
                    $lhs
                }
            }
        }
        
        match self.current_kind() {
            // prefix operation
            TokenKind::Operator => {
                if let Some(unary) = self.parse_builtin_unary(source_map) {
                    parse_potentially_infix!(unary)
                } else {
                    let operator = self.current().to_owned();
                    self.advance();

                    let operand = if let Some(unary) = self.parse_builtin_unary(source_map) {
                        unary
                    } else {
                        self.parse_call(source_map)
                    };

                    Expr::Prefix {
                        span: Span::new(operator.span().start(), operand.span().end(), operand.span().source_id()),
                        operator: Operation::Custom(operator),
                        operand: Box::new(operand)
                    }
                }
            }

            // ident as prefix operation
            TokenKind::Ident if 
                !matches!(self.peek_kind(), TokenKind::Operator |
                                            TokenKind::Dot      |
                                            TokenKind::Comma    |
                                            TokenKind::Semicolon|
                                            TokenKind::LParen   |   // `f (x)` is a function call.
                                            TokenKind::RParen   |   // To have it be an operation,
                                            TokenKind::LBracket |   // use `f {x}`.
                                            TokenKind::RBracket |
                                            TokenKind::RBrace   |
                                            TokenKind::Backtick |
                                            TokenKind::EOF)
                && !self.peek_kind().is_keyword() => {
                let operator = self.current().to_owned();
                self.advance();

                let operand = if let Some(unary) = self.parse_builtin_unary(source_map) {
                    unary
                } else {
                    self.parse_call(source_map)
                };

                Expr::Prefix {
                    span: Span::new(operator.span().start(), operand.span().end(), operand.span().source_id()),
                    operator: Operation::Ident(operator.try_into().unwrap()),
                    operand: Box::new(operand)
                }
            }

            // operator literal as prefix operation
            TokenKind::Backtick => {
                let span_start = self.current().span().start();
                self.advance();

                if let Ok(name) = self.parse_operator_literal() {
                    let operand = if let Some(unary) = self.parse_builtin_unary(source_map) {
                        unary
                    } else {
                        self.parse_call(source_map)
                    };
                    
                    Expr::Prefix {
                        span: Span::new(span_start, operand.span().end(), operand.span().source_id()),
                        operator: Operation::OpLit(name),
                        operand: Box::new(operand)
                    }
                } else {
                    todo!("after error with backtick")
                }
            }

            // potential ident/operation as infix operation
            _ => {
                let lhs = if let Some(unary) = self.parse_builtin_unary(source_map) {
                    unary
                } else {
                    self.parse_call(source_map)
                };

                parse_potentially_infix!(lhs)
            }
        }
    }

    fn parse_operator_literal(&mut self) -> Result<Token, ()> {
        if let TokenKind::Ident = self.current_kind() {
            let name = self.current().to_owned();
            self.advance();
            self.expect(TokenKind::Backtick);

            Ok(name)
        } else {
            todo!("report error for invalid operator literal")
        }
    }

    /// Attempts to parse a built-in unary expression. If it succeeds, it outputs the expression. If it cannot find a built-in unary operator, it returns None.
    fn parse_builtin_unary(&mut self, source_map: &SourceMap) -> Option<Expr> {
        if let Some(plus) = self.take_op(source_map, "+") {
            let expr = self.parse_call(source_map);

            Some(Expr::UnaryPlus {
                span: Span::new(plus.span().start(), expr.span().end(), expr.span().source_id()),
                expr: Box::new(expr),
            })
        } else if let Some(neg) = self.take_op(source_map, "-") {
            let expr = self.parse_call(source_map);

            Some(Expr::Neg {
                span: Span::new(neg.span().start(), expr.span().end(), expr.span().source_id()),
                expr: Box::new(expr)
            })
        } else if let Some(spread) = self.take_op(source_map, "...") {
            let expr = self.parse_call(source_map);

            Some(Expr::Spread {
                span: Span::new(spread.span().start(), expr.span().end(), expr.span().source_id()),
                expr: Box::new(expr)
            })
        } else {
            None
        }
    }

    /// Parses function calling, indexing, and dot access.
    fn parse_call(&mut self, source_map: &SourceMap) -> Expr {
        let mut expr = self.parse_grouping(source_map);

        loop {
            if self.accept(TokenKind::LParen) {
                expr = self.finish_call(source_map, expr)
            } else if self.accept(TokenKind::LBracket) {
                expr = self.finish_index(source_map, expr)
            } else if self.current().is_accessor(source_map) {
                expr = self.finish_access(source_map, expr)
            } else { // TODO:  dot access via a.b
                    // Both must be in this function because they must be same precedence as each other
                break expr
            }
        }
    }

    fn finish_call(&mut self, source_map: &SourceMap, callee: Expr) -> Expr {
        let mut args = vec![];
        let mut kwargs = vec![];
        let mut in_kwargs = false;

        if let Some(rp) = self.take(TokenKind::RParen) {            
            return Expr::Call {
                span: Span::new(callee.span().start(), rp.span().end(), rp.span().source_id()),
                callee: Box::new(callee),
                args,
                kwargs,
            };
        }

        loop {
            if args.len() >= Self::MAX_ARGS {
                todo!("too many arguments")
            }

            // kwarg
            if matches!(self.current_kind(), TokenKind::Ident) && matches!(self.peek_kind(), TokenKind::Eq) {
                in_kwargs = true;
                
                let arg = self.current().try_into().unwrap();
                
                self.advance();
                self.expect(TokenKind::Eq);

                let value = if let TokenKind::Comma | TokenKind::RParen = self.current_kind() {
                    Expr::Ident(arg)
                } else {
                    self.parse_expr(source_map)
                };

                kwargs.push((arg, value));
            // args
            } else if in_kwargs {
                todo!("positional arguments cannot appear after keyword arguments")
            } else {
                args.push(self.parse_expr(source_map))
            }

            if let Some(rp) = self.take(TokenKind::RParen) {                
                return Expr::Call {
                    span: Span::new(callee.span().start(), rp.span().end(), rp.span().source_id()),
                    callee: Box::new(callee),
                    args,
                    kwargs
                }
            } else if self.expect(TokenKind::Comma) {
                if let Some(rp) = self.take(TokenKind::RParen) {                    
                    return Expr::Call {
                        span: Span::new(callee.span().start(), rp.span().end(), rp.span().source_id()),
                        callee: Box::new(callee),
                        args,
                        kwargs
                    }
                }
            }
        }
    }

    fn finish_index(&mut self, source_map: &SourceMap, indexee: Expr) -> Expr {
        let mut args = vec![];

        if self.accept(TokenKind::RBracket) {
            todo!("index operation must have at least one argument")
        }

        loop {
            if args.len() > Self::MAX_ARGS {
                todo!("too many arguments")
            }

            args.push(self.parse_expr(source_map));

            if let Some(rb) = self.take(TokenKind::RBracket) {                
                return Expr::Index {
                    span: Span::new(indexee.span().start(), rb.span().end(), rb.span().source_id()),
                    indexee: Box::new(indexee),
                    args,
                }
            } else if self.expect(TokenKind::Comma) {
                if let Some(rb) = self.take(TokenKind::RBracket) {                    
                    return Expr::Index {
                        span: Span::new(indexee.span().start(), rb.span().end(), rb.span().source_id()),
                        indexee: Box::new(indexee),
                        args,
                    }
                }
            }
        }
    }

    fn finish_access(&mut self, source_map: &SourceMap, accessee: Expr) -> Expr {
        if self.accept(TokenKind::Dot) {
            if let Some(member) = self.take(TokenKind::Ident) {
                Expr::MemberAccess {
                    span: Span::new(accessee.span().start(), member.span().end(), member.span().source_id()),
                    accessee: Box::new(accessee),
                    member: member.try_into().unwrap()
                }
            } else {
                todo!("expected identifier")
            }
        } else if self.accept_op(source_map, ".@") {
            todo!("dot macro")
        } else {
            todo!()
        }
    }

    fn parse_grouping(&mut self, source_map: &SourceMap) -> Expr {
        if let Some(lp) = self.take(TokenKind::LParen) {
            let span_start = lp.span().start();

            if let Some(rp) = self.take(TokenKind::RParen) {
                Expr::Unit {
                    span: Span::new(span_start, rp.span().end(), rp.span().source_id())
                }
            } else {
                let mut expr = self.parse_expr(source_map);

                if let Some(rp) = self.take(TokenKind::RParen) {
                    expr.span_mut().set_start(span_start);
                    expr.span_mut().set_end(rp.span().end());
                    expr
                } else if self.expect(TokenKind::Comma) {
                    let mut exprs = vec![expr];

                    if let Some(rp) = self.take(TokenKind::RParen) {
                        Expr::Tuple {
                            exprs,
                            span: Span::new(span_start, rp.span().end(), rp.span().source_id()) 
                        }
                    } else {
                        loop {
                            exprs.push(self.parse_expr(source_map));

                            if let Some(rp) = self.take(TokenKind::RParen) {
                                break Expr::Tuple {
                                    exprs,
                                    span: Span::new(span_start, rp.span().end(), rp.span().source_id())
                                }
                            } else if self.expect(TokenKind::Comma) {
                                ()
                            } else {
                                todo!()
                            }
                        }
                    }
                } else {
                    todo!()
                }
            }
        } else {
            self.parse_def_in(source_map)
        }
    }

    fn parse_def_in(&mut self, source_map: &SourceMap) -> Expr {
        match self.current_kind() {
            TokenKind::Let => {
                let Stmt::Expr { expr: let_in, .. } = self.parse_let(source_map, true)
                else { todo!() };

                let_in
            }

            TokenKind::Var => {
                let Stmt::Expr { expr: var_in, .. } = self.parse_var(source_map, true)
                else { todo!() };

                var_in
            }

            TokenKind::Const => {
                let Stmt::Expr { expr: const_in, .. } = self.parse_const(source_map, true)
                else { todo!() };

                const_in
            }

            TokenKind::Fn => {
                let Stmt::Expr { expr: fn_in, .. } = self.parse_fn(source_map, true)
                else { todo!() };

                fn_in
            }

            _ => self.parse_primary(source_map)
        }
    }

    fn parse_primary(&mut self, source_map: &SourceMap) -> Expr {        
        match self.current_kind() {
            TokenKind::Int => {
                let number = self.current().get_lexeme(source_map).replace('_', "");
                let expr = Expr::Int {
                    value: Integer::parse(number).unwrap().into(),
                    span: self.current().span()
                };
                self.advance();

                expr
            }

            TokenKind::Real => {
                let mut reached_decimal = false;
                let mut denom_size = 1;
                let mut fraction = self
                    .current()
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
                    span: self.current().span()
                };
                self.advance();

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
                let lexeme = self.current().get_lexeme(source_map);
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
                    span: self.current().span()
                };
                self.advance();

                expr
            }

            TokenKind::Imag => {
                let lexeme = self.current().get_lexeme(source_map);

                if lexeme == "i" {
                    let expr = Expr::Imag {
                        value: Rational::ONE.to_owned(),
                        span: self.current().span()
                    };
                    self.advance();

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
                        span: self.current().span()
                    };
                    self.advance();

                    expr
                }
            }

            TokenKind::Ident => {
                let ident = Expr::Ident(self.current().try_into().unwrap());
                self.advance();

                ident
            }

            TokenKind::StringStart => {
                let span_start = self.current().span().start();
                let span_end;
                self.advance();

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
                            
                            parts.push(StringPart::Expr(self.parse_expr(source_map)));
                            self.advance();
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

                Expr::String {
                    parts,
                    span: Span::new(span_start, span_end, self.current().span().source_id())
                }
            }

            TokenKind::LBracket => {
                let span_start = self.current().span().start();
                self.advance();

                if let Some(rb) = self.take(TokenKind::RBracket) {
                    Expr::Array {
                        rows: vec![],
                        span: Span::new(span_start, rb.span().end(), rb.span().source_id())
                    }
                } else {
                    let mut rows = vec![];
                    let mut row = vec![];

                    loop {
                        row.push(self.parse_expr(source_map));

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

            // TODO: .

            _ => todo!("unknown primary expression starting at: {:?}", self.current_kind())
        }
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

    /// Advances to the next token in the token stream. If it is at end, it will keep yielding EOF.
    fn advance(&mut self) {
        self.current_token = self.tokens.next().unwrap();
    }

    fn at_end(&self) -> bool {
        self.current_kind() == TokenKind::EOF
    }

    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap()
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
+ - +- -+                       L
* / // % %%                     L
^                               R
user (like `1:f`)               N
unary                           _
index                           L
call                            L
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

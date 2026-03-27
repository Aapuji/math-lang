use std::iter::{Chain, Peekable, Repeat, repeat};
use std::os::unix::raw::pid_t;
use std::vec::IntoIter;

use rug::{Integer, Rational};

use crate::ast::{Expr, Operation, Stmt, StringPart, Type};
use crate::source::SourceMap;
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

        while !self.at_end() {            
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

    fn parse_non_expr_stmt(&mut self, source_map: &SourceMap) -> Stmt {
        match self.current_kind() {
            TokenKind::Let => self.parse_let(source_map),
            TokenKind::Var => self.parse_var(source_map),
            TokenKind::Const => self.parse_const(source_map),
            TokenKind::Fn => self.parse_fn(source_map),
            _ => todo!()
        }
    }

    fn parse_let(&mut self, source_map: &SourceMap) -> Stmt {
        self.accept(TokenKind::Let);

        if let TokenKind::Ident = self.current_kind() {
            let mut names = vec![*self.current()];
            self.advance();

            let args = if let TokenKind::LParen = self.current_kind() {
                Some(self.parse_args_def(source_map))
            } else { None };

            let ty = self.parse_type_annotation(source_map);

            let mut is_def = false;
            let def = if self.accept_op(source_map, "=") {
                Some(self.parse_expr(source_map))
            } else if self.accept_op(source_map, ":=") {
                is_def = true;
                Some(self.parse_expr(source_map))
            } else { None };

            if self.accept(TokenKind::In) {
                let expr = self.parse_expr(source_map);
                self.expect(TokenKind::Semicolon);

                if names.len() == 1 {
                    Stmt::Expr(if is_def {
                        Expr::DefIn {
                            name: names[0],
                            args, 
                            ty, 
                            def: Box::new(def.unwrap()),
                            expr: Box::new(expr)
                        }
                    } else {
                        Expr::LetIn {
                            name: names[0],
                            args,
                            ty,
                            value: def.map(Box::new),
                            expr: Box::new(expr)
                        }
                    })
                } else {
                    Stmt::Expr(if is_def {
                        Expr::DefManyIn {
                            names,
                            ty,
                            def: Box::new(def.unwrap()),
                            expr: Box::new(expr)
                        }
                    } else {
                        Expr::LetManyIn {
                            names,
                            ty,
                            value: def.map(Box::new),
                            expr: Box::new(expr)
                        }
                    })
                }
            } else {
                self.expect(TokenKind::Semicolon);

                if names.len() == 1 {
                    if is_def {
                        Stmt::Def {
                            name: names[0],
                            args,
                            ty,
                            def: def.unwrap()
                        }
                    } else {
                        Stmt::Let {
                            name: names[0],
                            args,
                            ty,
                            value: def
                        }
                    }
                } else {
                    if is_def {
                        Stmt::DefMany {
                            names,
                            ty,
                            def: def.unwrap()
                        }
                    } else {
                        Stmt::LetMany {
                            names,
                            ty,
                            value: def
                        }
                    }
                }
            }
        } else {
            todo!()
        }
    }

    fn parse_var(&mut self, source_map: &SourceMap) -> Stmt {        
        self.accept(TokenKind::Var);
        
        if let TokenKind::Ident = self.current_kind() {
            let name = *self.current();
            self.advance();

            let ty = self.parse_type_annotation(source_map);

            let def = if self.accept_op(source_map, "=") {
                Some(self.parse_expr(source_map))
            } else { None };

            if self.accept(TokenKind::In) {
                let expr = self.parse_expr(source_map);
                self.expect(TokenKind::Semicolon);

                Stmt::Expr(Expr::VarIn {
                    name,
                    ty,
                    value: def.map(Box::new),
                    expr: Box::new(expr)
                })
            } else {
                self.expect(TokenKind::Semicolon);
                Stmt::Var {
                    name,
                    ty,
                    value: def
                }
            }
        } else {
            todo!()
        }
    }

    fn parse_const(&mut self, source_map: &SourceMap) -> Stmt {        
        self.accept(TokenKind::Const);
        
        if let TokenKind::Ident = self.current_kind() {
            let name = *self.current();
            self.advance();

            let ty = self.parse_type_annotation(source_map);

            let def = if self.accept_op(source_map, "=") {
                Some(self.parse_expr(source_map))
            } else { None };

            if self.accept(TokenKind::In) {
                let expr = self.parse_expr(source_map);
                self.expect(TokenKind::Semicolon);

                Stmt::Expr(Expr::ConstIn {
                    name,
                    ty,
                    value: def.map(Box::new),
                    expr: Box::new(expr)
                })
            } else {
                self.expect(TokenKind::Semicolon);
                Stmt::Const {
                    name,
                    ty,
                    value: def
                }
            }
        } else {
            todo!()
        }
    }

    fn parse_fn(&mut self, source_map: &SourceMap) -> Stmt {
        self.accept(TokenKind::Fn);

        if let Some(name) = self.take(TokenKind::Ident) {
            // TODO: generic args
            self.expect(TokenKind::LParen);

            let args = self.parse_args_def(source_map);
            // self.expect(TokenKind::RParen);

            let ty = self.parse_type_annotation(source_map);

            let value = if self.accept_op(source_map, "=") {
                let expr = self.parse_expr(source_map);

                self.expect(TokenKind::Semicolon);
                expr
            } else {
                self.expect(TokenKind::LBrace);
                self.parse_block(source_map)
            };

            Stmt::Fn { name, args, ty, value }
        } else {
            todo!("error expected identifier")
        }
    }

    fn parse_args_def(&mut self, source_map: &SourceMap) -> Vec<(Token, Option<Type>)> {
        let mut args = vec![];
        
        loop {
            if let Some(arg) = self.take(TokenKind::Ident) {
                let ty = self.parse_type_annotation(source_map);
                args.push((arg, ty));

                if self.accept(TokenKind::RParen)
                    || (self.accept(TokenKind::Comma) && self.accept(TokenKind::RParen)) {
                    break
                }
            } else if self.accept(TokenKind::RParen) {
                break
            } else {
                todo!()
            }
        }

        // self.expect(TokenKind::RParen);
        args
    }

    /// Attempts to parse a type annotation. Either it will be a regular type annotation, or an implicit refinement type annotation, or there may be no type annotation, in which it will output None.
    fn parse_type_annotation(&mut self, source_map: &SourceMap) -> Option<Type> {
        // : T
        if self.accept_op(source_map, ":") {
            Some(self.parse_type(source_map))
        // :: r
        } else if self.accept_op(source_map, "::") {
            todo!()
        } else {
            None
        }
    }

    fn parse_type(&mut self, source_map: &SourceMap) -> Type {
        self.parse_primary_type(source_map)
    }

    fn parse_primary_type(&mut self, source_map: &SourceMap) -> Type {
        match self.current_kind() {
            TokenKind::Ident => {
                let ty = Type::Named(*self.current());
                self.advance();
                
                ty
            }

            _ => todo!()
        }
    }
    
    fn starts_non_expr_stmt(&self) -> bool {
        matches!(self.current_kind(), 
              TokenKind::Let
            | TokenKind::Var
            | TokenKind::Const
            | TokenKind::Fn
            | TokenKind::Sym
            | TokenKind::Alias
            | TokenKind::Using
        )
    }

    fn parse_expr_stmt(&mut self, source_map: &SourceMap) -> Stmt {
        let expr = self.parse_expr(source_map);

        if self.accept(TokenKind::Semicolon) || self.accept(TokenKind::EOF) {
            Stmt::Expr(expr)
        } else {
            todo!("report error - expected semicolon")
        }
    }

    fn parse_expr(&mut self, source_map: &SourceMap) -> Expr {
        if self.accept(TokenKind::LBrace) {
            self.parse_block(source_map)
        } else {
            self.parse_or(source_map)
        }
    }

    fn parse_block(&mut self, source_map: &SourceMap) -> Expr {
        let mut stmts = vec![];
        let mut tail = None;

        while self.current_kind() != TokenKind::RBrace {
            if self.starts_non_expr_stmt() {
                let stmt = self.parse_non_expr_stmt(source_map);
                stmts.push(stmt);
            } else {
                let expr = self.parse_expr(source_map);

                if self.accept(TokenKind::Semicolon) {
                    stmts.push(Stmt::Expr(expr));
                } else {
                    tail = Some(Box::new(expr));
                    break
                }
            }
        }

        self.expect(TokenKind::RBrace);
        Expr::Block { stmts, tail }
    }

    fn parse_or(&mut self, source_map: &SourceMap) -> Expr {
        let lhs = self.parse_xor(source_map);

        if self.accept(TokenKind::Or) {
            let rhs = Box::new(self.parse_or(source_map));

            Expr::Or {
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
                lhs: Box::new(lhs),
                rhs
            }
        } else {
            lhs
        }
    }

    fn parse_not(&mut self, source_map: &SourceMap) -> Expr {
        if self.accept(TokenKind::Not) {
            Expr::Not(Box::new(self.parse_not(source_map)))
        } else {
            self.parse_comparison(source_map)
        }
    }

    fn parse_comparison(&mut self, source_map: &SourceMap) -> Expr {
        let lhs = self.parse_additive(source_map);

        macro_rules! parse_comparison {
            ($node_kind:ident) => {
                {
                    let rhs = self.parse_comparison(source_map);
                    if let Some((lhsr, _)) = rhs.is_comparison_node() {
                        Expr::And {
                            lhs: Box::new(Expr::$node_kind {
                                lhs: Box::new(lhs),
                                rhs: lhsr.to_owned()
                            }),
                            rhs: Box::new(rhs)
                        }
                    } else if let Expr::And { lhs: ref lhsr, .. } = rhs {
                        if let Some((lhsr, _)) = lhsr.is_comparison_node() {
                            Expr::And {
                                lhs: Box::new(Expr::$node_kind {
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
        } else {
            lhs
        }
    }

    fn parse_additive(&mut self, source_map: &SourceMap) -> Expr {
        let lhs = self.parse_multiplicative(source_map);

        if self.accept_op(source_map, "+") {
            let rhs = Box::new(self.parse_additive(source_map));

            Expr::Plus {
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "-") {
            let rhs = Box::new(self.parse_additive(source_map));

            Expr::Minus {
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "+-") {
            let rhs = Box::new(self.parse_additive(source_map));

            Expr::PlusMinus {
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "-+") {
            let rhs = Box::new(self.parse_additive(source_map));

            Expr::MinusPlus {
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
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "/") {
            let rhs = Box::new(self.parse_multiplicative(source_map));

            Expr::Divide {
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "//") {
            let rhs = Box::new(self.parse_multiplicative(source_map));

            Expr::IntDivide {
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "%") {
            let rhs = Box::new(self.parse_multiplicative(source_map));

            Expr::Mod {
                lhs: Box::new(lhs),
                rhs
            }
        } else if self.accept_op(source_map, "%%") {
            let rhs = Box::new(self.parse_multiplicative(source_map));

            Expr::ModClass {
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
                        self.parse_index(source_map)
                    };

                    Expr::Infix {
                        lhs: Box::new($lhs),
                        operator: Operation::Ident(operator),
                        rhs: Box::new(rhs)
                    }
                } else if self.accept(TokenKind::Backtick) {
                    if let Ok(name) = self.parse_operator_literal(source_map, "2") {                        
                        let rhs = if let Some(unary) = self.parse_builtin_unary(source_map) {
                            unary
                        } else {
                            self.parse_index(source_map)
                        };

                        Expr::Infix {
                            lhs: Box::new($lhs),
                            operator: Operation::OpLit(name),
                            rhs: Box::new(rhs)
                        }
                    } else {
                        todo!("after error with backtick")
                    }
                } else if self.current().can_be_operator(source_map) && !self.current().is_builtin_operator(source_map) {
                    let operator = self.current().to_owned();
                    self.advance();

                    let rhs = if let Some(unary) = self.parse_builtin_unary(source_map) {
                        unary
                    } else {
                        self.parse_index(source_map)
                    };

                    Expr::Infix {
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

                    Expr::Prefix {
                        operator: Operation::Custom(operator),
                        operand: Box::new(if let Some(unary) = self.parse_builtin_unary(source_map) {
                            unary
                        } else {
                            self.parse_index(source_map)
                        })
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

                Expr::Prefix {
                    operator: Operation::Ident(operator),
                    operand: Box::new(if let Some(unary) = self.parse_builtin_unary(source_map) {
                        unary
                    } else {
                        self.parse_index(source_map)
                    })
                }
            }

            // operator literal as prefix operation
            TokenKind::Backtick => {
                self.advance();

                if let Ok(name) = self.parse_operator_literal(source_map, "1") {
                    Expr::Prefix {
                        operator: Operation::OpLit(name),
                        operand: Box::new(if let Some(unary) = self.parse_builtin_unary(source_map) {
                            unary
                        } else {
                            self.parse_index(source_map)
                        })
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
                    self.parse_index(source_map)
                };

                parse_potentially_infix!(lhs)
            }
        }
    }

    fn parse_operator_literal(&mut self, source_map: &SourceMap, req_arity: &str) -> Result<Token, ()> {
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
        if self.accept_op(source_map, "+") {
            Some(Expr::UnaryPlus(Box::new(self.parse_index(source_map))))
        } else if self.accept_op(source_map, "-") {
            Some(Expr::Neg(Box::new(self.parse_index(source_map))))
        } else if self.accept_op(source_map, "...") {
            Some(Expr::Spread(Box::new(self.parse_index(source_map))))
        } else {
            None
        }
    }

    fn parse_index(&mut self, source_map: &SourceMap) -> Expr {
        self.parse_call(source_map)
    }

    fn parse_call(&mut self, source_map: &SourceMap) -> Expr {
        let expr = self.parse_primary(source_map);

        if self.accept(TokenKind::LParen) {
            self.finish_call(source_map, expr)
        } else { // TODO: ident . smth
            expr
        }
    }

    fn finish_call(&mut self, source_map: &SourceMap, callee: Expr) -> Expr {
        let mut args = vec![];

        while !self.accept(TokenKind::RParen) {
            if args.len() >= Self::MAX_ARGS {
                todo!("too many arguments")
            }

            args.push(self.parse_expr(source_map));

            if self.accept(TokenKind::RParen) {
                break
            } else if !self.expect(TokenKind::Comma) {
                self.error(self.current_kind());
            }
        }

        Expr::Call {
            callee: Box::new(callee),
            args
        }
    }

    fn parse_primary(&mut self, source_map: &SourceMap) -> Expr {        
        match self.current_kind() {
            TokenKind::Int => {
                let number = self.current().get_lexeme(source_map).replace('_', "");
                let expr = Expr::Int(Integer::parse(number).unwrap().into());
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

                let expr = Expr::Real(Rational::parse(fraction).unwrap().into());
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

                let expr = Expr::Real(Rational::parse(fraction).unwrap().into());
                self.advance();

                expr
            }

            TokenKind::Imag => {
                let lexeme = self.current().get_lexeme(source_map);

                if lexeme == "i" {
                    let expr = Expr::Imag(Rational::ONE.to_owned());
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

                    let expr = Expr::Imag(Rational::parse(fraction).unwrap().into());
                    self.advance();

                    expr
                }
            }

            TokenKind::Ident => {
                let ident = Expr::Ident(*self.current());
                self.advance();

                ident
            }

            TokenKind::StringStart => {
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
                            
                            self.advance();
                            parts.push(StringPart::Expr(self.parse_expr(source_map)));
                        }

                        TokenKind::InterpolateEnd => {
                            self.advance();
                        },
                        
                        TokenKind::StringEnd => {
                            if !cur_text.is_empty() {
                                parts.push(StringPart::Text(cur_text));
                                cur_text = String::new();
                            }

                            self.advance();
                            break
                        }

                        TokenKind::Error(_) => todo!("parse error in string"),

                        _ => unreachable!()
                    }
                }

                Expr::String(parts)
            }

            TokenKind::LParen => {
                self.advance();

                if self.accept(TokenKind::RParen) {
                    Expr::Unit
                } else {
                    let expr = self.parse_expr(source_map);

                    if self.accept(TokenKind::RParen) {
                        expr
                    } else if self.expect(TokenKind::Comma) {
                        let mut exprs = vec![expr];

                        if self.accept(TokenKind::RParen) {
                            Expr::Tuple(exprs)
                        } else {
                            loop {
                                exprs.push(self.parse_expr(source_map));

                                if self.accept(TokenKind::RParen) {
                                    break Expr::Tuple(exprs)
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
            }

            _ => todo!("{:?}", self.current_kind())
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
            let token = *self.current();
            self.advance();
            
            Some(token)
        } else {
            None
        }
    }

    /// Checks if the current token matches the given `TokenKind`. If so, it advances to the next token and outputs the passed `Token`. If not, it outputs `None`.
    fn take_op(&mut self, source_map: &SourceMap, op: &str) -> Option<Token> {        
        if self.current_op(source_map)? == op {
            let token = *self.current();
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
            self.error(self.current_kind());
            false
        }
    }

    /// Expects that the current token is an operator and that it matches the given operator lexeme. If so, it advances and outputs true. Otherwise is reports an error and outputs `false`.
    fn expect_op(&mut self, source_map: &SourceMap, op: &str) -> bool {
        if self.accept_op(source_map, op) {
            true
        } else {
            self.error(self.current_kind());
            false
        }
    }

    fn require(&mut self, kind: TokenKind) -> Option<Token> {
        if let Some(token) = self.take(kind) {
            Some(token)
        } else {
            self.error(self.current_kind());
            None
        }
    }

    fn require_op(&mut self, source_map: &SourceMap, op: &str) -> Option<Token> {
        if let Some(token) = self.take_op(source_map, op) {
            Some(token)
        } else {
            self.error(self.current_kind());
            None
        }
    }

    fn error(&self, kind: TokenKind) {
        todo!("@ {:?}", kind)
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

    fn current(&self) -> &Token {
        &self.current_token
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

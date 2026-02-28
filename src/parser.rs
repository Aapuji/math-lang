use std::iter::{Chain, Peekable, Repeat, repeat};
use std::thread::current;
use std::vec::IntoIter;

use crate::ast::{Expr, Stmt, Type};
use crate::source::SourceMap;
use crate::token::{Token, TokenKind};

type TokenStream = Peekable<Chain<IntoIter<Token>, Repeat<Token>>>;

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: TokenStream,
    current_token: Token
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let last = tokens.last().cloned();
        let mut t = tokens
            .into_iter()
            .chain(repeat(last.unwrap()))
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
            self.advance();
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
            TokenKind::Var => self.parse_var(source_map),
            TokenKind::Const => self.parse_const(source_map),
            _ => todo!()
        }
    }

    fn parse_var(&mut self, source_map: &SourceMap) -> Stmt {        
        self.accept(TokenKind::Var);
        
        if let TokenKind::Ident = self.current_kind() {
            let name = self.current().clone();
            self.advance();

            let ty = if self.accept_op(source_map, ":") {
                Some(self.parse_type())
            } else { None };

            let def = if self.accept_op(source_map, "=") {
                Some(self.parse_expr(source_map))
            } else { None };

            if self.accept(TokenKind::In) {
                let expr = self.parse_expr(source_map);
                self.expect(TokenKind::Semicolon);

                Stmt::Expr(Expr::VarIn(name, ty, def.map(Box::new), Box::new(expr)))
            } else {
                Stmt::Var(name, ty, def)
            }
        } else {
            todo!()
        }
    }

    fn parse_const(&mut self, source_map: &SourceMap) -> Stmt {        
        self.accept(TokenKind::Const);
        
        if let TokenKind::Ident = self.current_kind() {
            let name = self.current().clone();
            self.advance();

            let ty = if self.accept_op(source_map, ":") {
                Some(self.parse_type())
            } else { None };

            let def = if self.accept_op(source_map, "=") {
                Some(self.parse_expr(source_map))
            } else { None };

            if self.accept(TokenKind::In) {
                let expr = self.parse_expr(source_map);
                self.expect(TokenKind::Semicolon);

                Stmt::Expr(Expr::ConstIn(name, ty, def.map(Box::new), Box::new(expr)))
            } else {
                Stmt::Const(name, ty, def)
            }
        } else {
            todo!()
        }
    }

    fn parse_type(&mut self) -> Type {
        self.parse_primary_type()
    }

    fn parse_primary_type(&mut self) -> Type {
        match self.current_kind() {
            TokenKind::Ident => {
                let ty = Type::Named(*self.current());
                self.accept(TokenKind::Ident);
                ty
            }

            _ => todo!()
        }
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
            self.parse_primary()
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

    fn parse_block(&mut self, source_map: &SourceMap) -> Expr {
        let mut stmts = vec![];
        let mut tail = None;

        while self.current_kind() != TokenKind::RBrace {
            if self.starts_non_expr_stmt() {
                let stmt = self.parse_non_expr_stmt(source_map);
                self.expect(TokenKind::Semicolon);
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
        Expr::Block(stmts, tail)
    }

    fn parse_primary(&mut self) -> Expr {
        match self.current_kind() {
            TokenKind::Int => {
                let expr = Expr::Int(*self.current());
                self.advance();

                expr
            }

            TokenKind::Real => {
                let expr = Expr::Int(*self.current());
                self.advance();

                expr
            }

            TokenKind::Complex => {
                let expr = Expr::Int(*self.current());
                self.advance();

                expr
            }

            TokenKind::Ident => {
                let ident = Expr::Ident(*self.current());
                self.advance();

                ident
            }

            TokenKind::String => {
                let string = Expr::String(*self.current());
                self.advance();

                string
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
    fn expect_op(&mut self, source_map: &mut SourceMap, op: &str) -> bool {
        if self.accept_op(source_map, op) {
            true
        } else {
            self.error(self.current_kind());
            false
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


// TODO: Error detection and synchronization

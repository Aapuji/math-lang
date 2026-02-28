use std::iter::{Chain, Peekable, Repeat, repeat};
use std::vec::IntoIter;

use crate::ast::{Expr, Stmt};
use crate::source::SourceMap;
use crate::token::{Token, TokenKind};

type TokenStream = Peekable<Chain<IntoIter<Token>, Repeat<Token>>>;

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: TokenStream,
    current_token: Token
}

macro_rules! accept_match {
    ($self:expr, $value:expr,
        $( $pat:pat $(if $guard:expr)? => $body:expr ),+ $(,)?
    ) => {
        match $value {
            $(
                $pat $(if $guard)? => {
                    let body = $body;
                    $self.accept($value);
                    body
                }
            ),+
        }
    };
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
            stmts.push(self.parse_stmt());
            self.advance();
        }

        stmts
    }

    fn parse_stmt(&mut self) -> Stmt {
        if self.starts_non_expr_stmt() {
            self.parse_non_expr_stmt()
        } else {
            self.parse_expr_stmt()
        }
    }

    fn parse_non_expr_stmt(&mut self) -> Stmt {
        match self.current_kind() {
            _ => todo!(),
        }
    }

    fn parse_expr_stmt(&mut self) -> Stmt {
        let expr = self.parse_expr();

        if self.accept(TokenKind::Semicolon) || self.accept(TokenKind::EOF) {
            Stmt::Expr(expr)
        } else {
            todo!("report error - expected semicolon")
        }
    }

    fn parse_expr(&mut self) -> Expr {
        if self.accept(TokenKind::LBrace) {
            self.parse_block()
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

    fn parse_block(&mut self) -> Expr {
        let mut stmts = vec![];
        let mut tail = None;

        while self.current_kind() != TokenKind::RBrace {
            if self.starts_non_expr_stmt() {
                let stmt = self.parse_non_expr_stmt();
                self.expect(TokenKind::Semicolon);
                stmts.push(stmt);
            } else {
                let expr = self.parse_expr();

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
        accept_match! { self, self.current_kind(),
            TokenKind::Int => Expr::Int(*self.current()),
            TokenKind::Real => Expr::Real(*self.current()),
            TokenKind::Complex => Expr::Complex(*self.current()),
            _ => todo!()
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

    fn error(&mut self, kind: TokenKind) {
        todo!("@ {:?}", kind)
    }

    fn error_at(&mut self, token: &Token) {
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

    /// Checks if peeked token matches the given `TokenKind`.
    fn check(&mut self, kind: TokenKind) -> bool {
        self.peek_kind() == kind
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

use crate::token::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Int(Token),
    Real(Token),
    Complex(Token),
    Block(Vec<Stmt>, Option<Box<Expr>>)
}

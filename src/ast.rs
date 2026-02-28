use crate::token::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Var(Token, Option<Type>, Option<Expr>),
    Const(Token, Option<Type>, Option<Expr>),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Ident(Token),
    String(Token),
    Int(Token),
    Real(Token),
    Complex(Token),
    Block(Vec<Stmt>, Option<Box<Expr>>),
    VarIn(Token, Option<Type>, Option<Box<Expr>>, Box<Expr>),
    ConstIn(Token, Option<Type>, Option<Box<Expr>>, Box<Expr>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Named(Token),
    // more
}


// TODO: Perhaps instead of calling `accept` before entering the individual parse functions, have the accept in the parse functions. Becaus then in the future if there is an error that originates at the initial token which gets accepted, we can store that.

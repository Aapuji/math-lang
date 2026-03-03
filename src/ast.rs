use crate::token::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Let(Token, Option<Vec<(Token, Option<Type>)>>, Option<Type>, Option<Expr>),
    LetMany(Vec<Token>, Option<Type>, Option<Expr>),
    Def(Token, Option<Vec<(Token, Option<Type>)>>, Option<Type>, Expr),
    DefMany(Vec<Token>, Option<Type>, Expr),
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
    Call(Box<Expr>, Vec<Expr>),
    LetIn(Token, Option<Vec<(Token, Option<Type>)>>, Option<Type>, Option<Box<Expr>>, Box<Expr>),
    LetManyIn(Vec<Token>, Option<Type>, Option<Box<Expr>>, Box<Expr>),
    DefIn(Token, Option<Vec<(Token, Option<Type>)>>, Option<Type>, Box<Expr>, Box<Expr>),
    DefManyIn(Vec<Token>, Option<Type>, Box<Expr>, Box<Expr>),
    VarIn(Token, Option<Type>, Option<Box<Expr>>, Box<Expr>),
    ConstIn(Token, Option<Type>, Option<Box<Expr>>, Box<Expr>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Named(Token),
    // more
}

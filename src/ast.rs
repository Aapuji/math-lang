use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        name: Token,
        args: Option<Vec<(Token, Option<Type>)>>,
        ty: Option<Type>,
        value: Option<Expr>
    },
    LetFn {
        name: Token,
        ty_args: Option<Vec<Generic>>,
        args: Vec<(Token, Option<Type>)>,
        ty: Option<Type>,
        value: Option<Expr>
    },
    LetMany {
        names: Vec<Token>,
        ty: Option<Type>,
        value: Option<Expr>
    },
    Def {
        name: Token,
        args: Option<Vec<(Token, Option<Type>)>>,
        ty: Option<Type>,
        def: Expr
    },
    DefFn {
        name: Token,
        ty_args: Option<Vec<Generic>>,
        args: Vec<(Token, Option<Type>)>,
        def: Expr
    },
    DefMany {
        names: Vec<Token>,
        ty: Option<Type>,
        def: Expr
    },
    Var {
        name: Token,
        ty: Option<Type>,
        value: Option<Expr>
    },
    Const {
        name: Token,
        ty: Option<Type>,
        value: Option<Expr>
    },
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Generic {
    name: Token
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(Token),
    String(Vec<StringPart>),
    Int(rug::Integer),
    Real(rug::Rational),
    Imag(rug::Rational),
    Block {
        stmts: Vec<Stmt>,
        tail: Option<Box<Expr>>
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>
    },
    LetIn {
        name: Token,
        args: Option<Vec<(Token, Option<Type>)>>,
        ty: Option<Type>,
        value: Option<Box<Expr>>,
        expr: Box<Expr>
    },
    LetManyIn {
        names: Vec<Token>,
        ty: Option<Type>,
        value: Option<Box<Expr>>,
        expr: Box<Expr>
    },
    DefIn {
        name: Token,
        args: Option<Vec<(Token, Option<Type>)>>,
        ty: Option<Type>,
        def: Box<Expr>,
        expr: Box<Expr>
    },
    DefManyIn {
        names: Vec<Token>,
        ty: Option<Type>,
        def: Box<Expr>,
        expr: Box<Expr>
    },
    VarIn {
        name: Token,
        ty: Option<Type>,
        value: Option<Box<Expr>>,
        expr: Box<Expr>
    },
    ConstIn {
        name: Token,
        ty: Option<Type>,
        value: Option<Box<Expr>>,
        expr: Box<Expr>
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Named(Token),
    // more
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    Text(String),        // converts escape sequences
    Expr(Expr)
}

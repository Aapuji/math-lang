use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        name: Token,
        args: Option<Vec<(Token, Option<Type>)>>,
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
    Fn {
        name: Token,
        args: Vec<(Token, Option<Type>)>,
        ty: Option<Type>,
        value: Expr
    },
    Expr(Expr),
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
    Or {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Xor {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    And {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Not(Box<Expr>),
    Eq {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    NotEq {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Less {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Greater {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    LessEq {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    GreaterEq {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Plus {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Minus {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    PlusMinus {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    MinusPlus {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Times {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Divide {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    IntDivide {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Mod {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    ModClass {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    }, 
    Exp {
        lhs: Box<Expr>,
        rhs: Box<Expr>
    },
    Prefix {
        operator: Operation,
        operand: Box<Expr>
    },
    Infix {
        lhs: Box<Expr>,
        operator: Operation,
        rhs: Box<Expr>
    },
    UnaryPlus(Box<Expr>),
    Neg(Box<Expr>),
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>
    },
    Unit,
    Tuple(Vec<Expr>),
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

impl Expr {
    pub fn is_comparison_node(&self) -> Option<(&Box<Expr>, &Box<Expr>)> {
        match self {
            Expr::Eq { lhs, rhs }        |
            Expr::NotEq { lhs, rhs }     |
            Expr::Less { lhs, rhs }      |
            Expr::Greater { lhs, rhs }   |
            Expr::LessEq { lhs, rhs }    |
            Expr::GreaterEq { lhs, rhs } => Some((lhs, rhs)),
            _ => None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Named(Token),
    // more
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operation {
    Ident(Token),
    Custom(Token),
    OpLit(Token),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    Text(String),        // converts escape sequences
    Expr(Expr)
}

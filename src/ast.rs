use crate::{source::{SourceMap, Span}, token::{Token, TokenKind}};

// TODO: store spans

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Let(Let),
    Var {
        name: Var,
        ty: Option<Type>,
        value: Option<Expr>
    },
    Const {
        name: Var,
        ty: Option<Type>,
        value: Option<Expr>
    },
    Fn {
        header: FnHeader,
        value: Expr
    },
    Enum {
        name: Var,
        ty_args: Vec<Generic>,
        variants: Vec<Variant>
    },
    Struct {
        name: Var,
        ty_args: Vec<Generic>,
        fields: Vec<(Var, Type)>
    },
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Ident(Var),
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
    In {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
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
    Range {
        lhs: Endpoint,
        rhs: Endpoint,
        step: RangeStep
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
    Spread(Box<Expr>),
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        kwargs: Vec<(Var, Expr)>
    },
    Unit,
    Tuple(Vec<Expr>),
    LetIn(Box<Let>, Box<Expr>),
    VarIn {
        name: Var,
        ty: Option<Type>,
        value: Option<Box<Expr>>,
        expr: Box<Expr>
    },
    ConstIn {
        name: Var,
        ty: Option<Type>,
        value: Option<Box<Expr>>,
        expr: Box<Expr>
    },
    FnIn {
        header: FnHeader,
        value: Box<Expr>,
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

// TODO: symbol id
type SymbolId = usize;

/// Represents a name. `SymbolId` is assigned to 0 until name resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Var {
    id: SymbolId,
    span: Span
}

impl Var {
    pub fn new(span: Span) -> Self {
        Self {
            id: 0,
            span
        }
    }

    pub fn get_lexeme<'s>(&self, source_map: &'s SourceMap) -> &'s str {
        self.span.get_lexeme(source_map)
    }
}

impl TryFrom<Token> for Var {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.kind() {
            TokenKind::Ident => Ok(Var::new(value.span())),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Let {
    bindings: Vec<Binding>,
    kind: LetKind,
    value: Option<Expr>,
}

impl Let {
    pub fn new(bindings: Vec<Binding>, kind: LetKind, value: Option<Expr>) -> Self {
        Self { bindings, kind, value }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LetKind {
    Assign,
    Define,
    Declare
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binding {
    Name(Var, Option<Type>),
    Call(FnHeader),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnHeader {
    name: Var,
    ty_args: Vec<Generic>,
    args: Vec<(Var, Option<Type>)>,
    kwargs: Vec<(Var, Option<Type>)>,
    ty: Option<Type>
}

impl FnHeader {
    pub fn new(name: Var, ty_args: Vec<Generic>, args: Vec<(Var, Option<Type>)>, kwargs: Vec<(Var, Option<Type>)>, ty: Option<Type>) -> Self {
        Self { name, ty_args, args, kwargs, ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Named(Var),
    // more
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Generic {
    pub name: Var,
    // pub sat: Option<Var>      // TODO: figure out if we are going to be doing a sat system or impl or whatnot
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Variant {
    Const(Var),
    Tuple(Vec<Type>),
    Record(Vec<(Var, Type)>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Endpoint {
    Inclusive(Box<Expr>),
    Exclusive(Box<Expr>),
    Unspecified
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RangeStep {
    Discrete(Box<Expr>),
    Continuous
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Alias {
    new: Var,
    old: AliasSrc,
    kind: AliasKind
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AliasSrc {
    Ident(Var),
    Operator(Operation),
    Expr(Expr)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AliasKind {
    Ident,
    Operator
}

// TODO: this
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Macro {
    name: Var,
    arity: u8,

    // block: Expr
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operation {
    Ident(Var),
    Custom(Token),
    OpLit(Token),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StringPart {
    Text(String),        // converts escape sequences
    Expr(Expr)
}

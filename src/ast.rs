use crate::{source::{SourceMap, Span}, token::{Token, TokenKind}};

// TODO: store spans

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Let {
        def: Let,
        span: Span
    },
    Var {
        name: Var,
        ty: Option<Type>,
        value: Option<Expr>,
        span: Span
    },
    Const {
        name: Var,
        ty: Option<Type>,
        value: Option<Expr>,
        span: Span
    },
    Fn {
        header: FnHeader,
        value: Expr,
        span: Span
    },
    Sym {
        name: Var,
        args: Vec<(Var, Option<Type>)>,
        ty: Option<Type>,
        span: Span
    },
    Enum {
        name: Var,
        ty_args: Vec<Generic>,
        variants: Vec<Variant>,
        span: Span
    },
    Struct {
        name: Var,
        ty_args: Vec<Generic>,
        fields: Vec<(Var, Type)>,
        span: Span
    },
    Expr {
        expr: Expr,
        span: Span
    }
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Self::Let { span, .. } => *span,
            Self::Var { span, .. } => *span,
            Self::Const { span, .. } => *span,
            Self::Fn { span, .. } => *span,
            Self::Sym { span, .. } => *span, 
            Self::Enum { span, .. } => *span,
            Self::Struct { span, .. } => *span,
            Self::Expr { span, .. } => *span,
        }
    }

    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Let { span, .. } => &mut *span,
            Self::Var { span, .. } => &mut *span,
            Self::Const { span, .. } => &mut *span,
            Self::Fn { span, .. } => &mut *span,
            Self::Sym { span, .. } => &mut *span,
            Self::Enum { span, .. } => &mut *span,
            Self::Struct { span, .. } => &mut *span,
            Self::Expr { span, .. } => &mut *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Ident(Var),
    String {
        parts: Vec<StringPart>,
        span: Span
    },
    Int {
        value: rug::Integer,
        span: Span
    },
    Real {
        value: rug::Rational,
        span: Span
    },
    Imag {
        value: rug::Rational,
        span: Span
    },
    Array {
        rows: Vec<Vec<Expr>>,
        span: Span
    },
    Block {
        stmts: Vec<Stmt>,
        tail: Option<Box<Expr>>,
        span: Span
    },
    Or {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    Xor {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    And {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    Not {
        expr: Box<Expr>,
        span: Span
    },
    Eq {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    NotEq {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    Less {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    Greater {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    LessEq {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    GreaterEq {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    In {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    Plus {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    Minus {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    PlusMinus {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    MinusPlus {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    Times {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    Divide {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    IntDivide {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    Mod {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    ModClass {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    }, 
    Exp {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span
    },
    Range {
        lhs: Endpoint,
        rhs: Endpoint,
        step: RangeStep,
        span: Span
    },
    Prefix {
        operator: Operation,
        operand: Box<Expr>,
        span: Span
    },
    Infix {
        lhs: Box<Expr>,
        operator: Operation,
        rhs: Box<Expr>,
        span: Span
    },
    UnaryPlus {
        expr: Box<Expr>,
        span: Span
    },
    Neg {
        expr: Box<Expr>,
        span: Span
    },
    Spread {
        expr: Box<Expr>,
        span: Span
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        kwargs: Vec<(Var, Expr)>,
        span: Span
    },
    Index {
        indexee: Box<Expr>,
        args: Vec<Expr>,
        span: Span
    },
    MemberAccess {
        accessee: Box<Expr>,
        member: Var,
        span: Span
    },
    Unit {
        span: Span
    },
    Tuple {
        exprs: Vec<Expr>,
        span: Span
    },
    LetIn {
        def: Box<Let>,
        expr: Box<Expr>,
        span: Span
    },
    VarIn {
        name: Var,
        ty: Option<Type>,
        value: Option<Box<Expr>>,
        expr: Box<Expr>,
        span: Span
    },
    ConstIn {
        name: Var,
        ty: Option<Type>,
        value: Option<Box<Expr>>,
        expr: Box<Expr>,
        span: Span
    },
    FnIn {
        header: FnHeader,
        value: Box<Expr>,
        expr: Box<Expr>,
        span: Span
    }
}

impl Expr {
    pub fn is_comparison_node(&self) -> Option<(&Box<Expr>, &Box<Expr>)> {
        match self {
            Expr::Eq { lhs, rhs, span: _ }        |
            Expr::NotEq { lhs, rhs, span: _ }     |
            Expr::Less { lhs, rhs, span: _ }      |
            Expr::Greater { lhs, rhs, span: _ }   |
            Expr::LessEq { lhs, rhs, span: _ }    |
            Expr::GreaterEq { lhs, rhs, span: _ } => Some((lhs, rhs)),
            _ => None
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Ident(var) => var.span,
            Self::String { span, .. } => *span,
            Self::Int { span, .. } => *span,
            Self::Real { span, .. } => *span,
            Self::Imag { span, .. } => *span,
            Self::Array { span, .. } => *span,
            Self::Block { span, .. } => *span,
            Self::Or { span, .. } => *span,
            Self::Xor { span, .. } => *span,
            Self::And { span, .. } => *span,
            Self::Not { span, .. } => *span,
            Self::Eq { span, .. } => *span,
            Self::NotEq { span, .. } => *span,
            Self::Less { span, .. } => *span,
            Self::Greater { span, .. } => *span,
            Self::LessEq { span, .. } => *span,
            Self::GreaterEq { span, .. } => *span,
            Self::In { span, .. } => *span,
            Self::Plus { span, .. } => *span,
            Self::Minus { span, .. } => *span,
            Self::PlusMinus { span, .. } => *span,
            Self::MinusPlus { span, .. } => *span,
            Self::Times { span, .. } => *span,
            Self::Divide { span, .. } => *span,
            Self::IntDivide { span, .. } => *span,
            Self::Mod { span, .. } => *span,
            Self::ModClass { span, .. } => *span,
            Self::Exp { span, .. } => *span,
            Self::Range { span, .. } => *span,
            Self::Prefix { span, .. } => *span,
            Self::Infix { span, .. } => *span,
            Self::UnaryPlus { span, .. } => *span,
            Self::Neg { span, .. } => *span,
            Self::Spread { span, .. } => *span,
            Self::Call { span, .. } => *span,
            Self::MemberAccess { span, .. } => *span,
            Self::Index { span, .. } => *span,
            Self::Unit { span, .. } => *span,
            Self::Tuple { span, .. } => *span,
            Self::LetIn { span, .. } => *span,
            Self::VarIn { span, .. } => *span,
            Self::ConstIn { span, .. } => *span,
            Self::FnIn { span, .. } => *span,
        }
    }

        pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Ident(var) => &mut var.span,
            Self::String { span, .. } => &mut *span,
            Self::Int { span, .. } => &mut *span,
            Self::Real { span, .. } => &mut *span,
            Self::Imag { span, .. } => &mut *span,
            Self::Array { span, .. } => &mut *span,
            Self::Block { span, .. } => &mut *span,
            Self::Or { span, .. } => &mut *span,
            Self::Xor { span, .. } => &mut *span,
            Self::And { span, .. } => &mut *span,
            Self::Not { span, .. } => &mut *span,
            Self::Eq { span, .. } => &mut *span,
            Self::NotEq { span, .. } => &mut *span,
            Self::Less { span, .. } => &mut *span,
            Self::Greater { span, .. } => &mut *span,
            Self::LessEq { span, .. } => &mut *span,
            Self::GreaterEq { span, .. } => &mut *span,
            Self::In { span, .. } => &mut *span,
            Self::Plus { span, .. } => &mut *span,
            Self::Minus { span, .. } => &mut *span,
            Self::PlusMinus { span, .. } => &mut *span,
            Self::MinusPlus { span, .. } => &mut *span,
            Self::Times { span, .. } => &mut *span,
            Self::Divide { span, .. } => &mut *span,
            Self::IntDivide { span, .. } => &mut *span,
            Self::Mod { span, .. } => &mut *span,
            Self::ModClass { span, .. } => &mut *span,
            Self::Exp { span, .. } => &mut *span,
            Self::Range { span, .. } => &mut *span,
            Self::Prefix { span, .. } => &mut *span,
            Self::Infix { span, .. } => &mut *span,
            Self::UnaryPlus { span, .. } => &mut *span,
            Self::Neg { span, .. } => &mut *span,
            Self::Spread { span, .. } => &mut *span,
            Self::Call { span, .. } => &mut *span,
            Self::Index { span, .. } => &mut *span,
            Self::MemberAccess { span, .. } => &mut *span,
            Self::Unit { span, .. } => &mut *span,
            Self::Tuple { span, .. } => &mut *span,
            Self::LetIn { span, .. } => &mut *span,
            Self::VarIn { span, .. } => &mut *span,
            Self::ConstIn { span, .. } => &mut *span,
            Self::FnIn { span, .. } => &mut *span,
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

    pub fn span(&self) -> Span {
        self.span
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
    Fn(FnHeader),   // when it is known that it is a function binding
    Call(FnHeader), // when it is unknown whether it is a function or constructor binding
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnHeader {
    name: Var,
    ty_args: Vec<Generic>,
    args: Vec<(Var, Option<Type>)>,
    kwargs: Vec<(Var, Option<Type>)>,
    ty: Option<Type>,
    span: Span
}

impl FnHeader {
    pub fn new(
        name: Var, 
        ty_args: Vec<Generic>, 
        args: Vec<(Var, Option<Type>)>, 
        kwargs: Vec<(Var, Option<Type>)>, 
        ty: Option<Type>, 
        span: Span
    ) -> Self {
        Self { name, ty_args, args, kwargs, ty, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit {
        span: Span
    },
    Named(Var),
    Array {
        shape: Shape,
        ty: Box<Type>,
        span: Span
    },
    Tuple {
        types: Vec<Type>,
        span: Span
    }
    // more
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Unit { span } => *span,
            Type::Named(var) => var.span,
            Type::Array { span, .. } => *span,
            Type::Tuple { span, .. } => *span
        }
    }

    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Type::Unit { span } => &mut *span,
            Type::Named(var) => &mut var.span,
            Type::Array { span, .. } => &mut *span,
            Type::Tuple { span, .. } => &mut *span
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Shape {
    Empty,
    Dynamic,
    Specified(Vec<ShapeSpec>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShapeSpec {
    Known(Expr), // TODO: Perhaps split into KnownValue(Int) and KnownName(Var)
    Unknown
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

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct Alias {
//     new: Var,
//     old: AliasSrc,
//     kind: AliasKind
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum AliasSrc {
//     Ident(Var),
//     Operator(Operation),
//     Expr(Expr)
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum AliasKind {
//     Ident,
//     Operator
// }

// // TODO: this
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct Macro {
//     name: Var,
//     arity: u8,

//     // block: Expr
// }

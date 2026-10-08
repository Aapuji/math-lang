use rug::Integer;

use crate::source::{SourceMap, Span};
use crate::token::{Token, TokenKind, LexemeId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Type {
        name: Var,
        ty_args: Vec<Generic>,
        def: Type,
        span: Span
    },
    Alias {
        new: AliasLeft,
        old: AliasRight,
        span: Span
    },
    For {
        binding: Binding,
        expr: Expr,
        body: Expr,
        span: Span
    },
    While {
        cond: Expr,
        body: Expr,
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
            Self::Type { span, .. } => *span,
            Self::Alias { span, .. } => *span,
            Self::For { span, .. } => *span,
            Self::While { span, .. } => *span,
            Self::Expr { span, .. } => *span
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
            Self::Type { span, .. } => &mut *span,
            Self::Alias { span, .. } => &mut *span,
            Self::For { span, .. } => &mut *span,
            Self::While { span, .. } => &mut *span,
            Self::Expr { span, .. } => &mut *span
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Ident(Var),
    String {
        parts: Vec<StringPart>,
        span: Span
    },
    Latex(Box<Expr>),
    Int {
        value: AstInt,
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
    True(Span),
    False(Span),
    Grouping {
        expr: Box<Expr>,
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
    Operations {    // Thoughts: After name resolution, a pass is done to resolve this node into the concrete operation nodes
                    // Additionally, it can perform some form of disambiguation, since all operators are built-in at that stage
                    // So it can take the longest possible valid operator, etc.
                    // For example, ::- would take :: as it is the longest builtin operator, and then - as the longest one after that.
        items: Vec<OperationItem>,
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
    Mod {   // x % y outputs a mod class type (ie. 5 %2 + 3 == 0). To only get rem, cast back to Int (x % y as Int).
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
    // CApply {
    //     lhs: Var,
    //     args: Vec<Expr>,
    //     kwargs: Vec<(Var, Expr)>,
    //     span: Span
    // },
    // IApply {
    //     lhs: Var,
    //     args: Vec<Expr>,
    //     span: Span
    // },
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
    If {
        cond: Box<Expr>,
        if_body: Box<Expr>,
        else_body: Option<Box<Expr>>,
        span: Span
    },
    Match {
        value: Box<Expr>,
        arms: Vec<(Pattern, Option<PatternGuard>, Expr)>,
        span: Span
    },
    // MatchSym {
    //     value: Box<Expr>,
    //     arms: Vec<(SymPattern, Option<PatternGuard>, Expr)>,
    //     span: Span
    // },
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
            Self::Latex(expr) => expr.span(),
            Self::Int { span, .. } => *span,
            Self::Real { span, .. } => *span,
            Self::Imag { span, .. } => *span,
            Self::True(span) => *span,
            Self::False(span) => *span,
            Self::Grouping { span, .. } => *span,
            Self::Array { span, .. } => *span,
            Self::Block { span, .. } => *span,
            Self::Operations { span, .. } => *span,
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
            Self::Exp { span, .. } => *span,
            Self::Range { span, .. } => *span,
            Self::Prefix { span, .. } => *span,
            Self::Infix { span, .. } => *span,
            Self::UnaryPlus { span, .. } => *span,
            Self::Neg { span, .. } => *span,
            Self::Spread { span, .. } => *span,
            // Self::Apply { span, .. } => *span,
            Self::Call { span, .. } => *span,
            Self::MemberAccess { span, .. } => *span,
            Self::Index { span, .. } => *span,
            Self::Unit { span, .. } => *span,
            Self::Tuple { span, .. } => *span,
            Self::If { span, .. } => *span,
            Self::Match { span, .. } => *span,
            // Self::MatchSym { span, .. } => *span,
            Self::LetIn { span, .. } => *span,
            Self::VarIn { span, .. } => *span,
            Self::ConstIn { span, .. } => *span,
            Self::FnIn { span, .. } => *span,
        }
    }

        pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Ident(var) => &mut var.span,
            Self::String { span, .. } => span,
            Self::Latex(expr) => expr.span_mut(),
            Self::Int { span, .. } => span,
            Self::Real { span, .. } => span,
            Self::Imag { span, .. } => span,
            Self::True(span) => span,
            Self::False(span) => span,
            Self::Grouping { span, .. } => span,
            Self::Array { span, .. } => span,
            Self::Block { span, .. } => span,
            Self::Operations { span, .. } => span,
            Self::Or { span, .. } => span,
            Self::Xor { span, .. } => span,
            Self::And { span, .. } => span,
            Self::Not { span, .. } => span,
            Self::Eq { span, .. } => span,
            Self::NotEq { span, .. } => span,
            Self::Less { span, .. } => span,
            Self::Greater { span, .. } => span,
            Self::LessEq { span, .. } => span,
            Self::GreaterEq { span, .. } => span,
            Self::In { span, .. } => span,
            Self::Plus { span, .. } => span,
            Self::Minus { span, .. } => span,
            Self::PlusMinus { span, .. } => span,
            Self::MinusPlus { span, .. } => span,
            Self::Times { span, .. } => span,
            Self::Divide { span, .. } => span,
            Self::IntDivide { span, .. } => span,
            Self::Mod { span, .. } => span,
            Self::Exp { span, .. } => span,
            Self::Range { span, .. } => span,
            Self::Prefix { span, .. } => span,
            Self::Infix { span, .. } => span,
            Self::UnaryPlus { span, .. } => span,
            Self::Neg { span, .. } => span,
            Self::Spread { span, .. } => span,
            // Self::Apply { span, .. } => span,
            Self::Call { span, .. } => span,
            Self::Index { span, .. } => span,
            Self::MemberAccess { span, .. } => span,
            Self::Unit { span, .. } => span,
            Self::Tuple { span, .. } => span,
            Self::If { span, .. } => span,
            Self::Match { span, .. } => span,
            // Self::MatchSym { span, .. } => span,
            Self::LetIn { span, .. } => span,
            Self::VarIn { span, .. } => span,
            Self::ConstIn { span, .. } => span,
            Self::FnIn { span, .. } => span,
        }
    }
}

type SymbolId = usize;

#[derive(Debug, Clone, PartialEq, Eq, Ord, Hash)]
pub enum AstInt {
    Small(u32),
    Large(rug::Integer)
}

impl Into<AstInt> for u32 {
    fn into(self) -> AstInt {
        if self > 0x7FFF_FFFF {
            AstInt::Large(Integer::from(self))
        } else {
            AstInt::Small(self)
        }
    }
}

impl PartialOrd for AstInt {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (AstInt::Small(x), AstInt::Small(y)) => x.partial_cmp(y),
            (AstInt::Small(x), AstInt::Large(y)) => x.partial_cmp(y),
            (AstInt::Large(x), AstInt::Small(y)) => x.partial_cmp(y),
            (AstInt::Large(x), AstInt::Large(y)) => x.partial_cmp(y)
        }
    }
}

/// Represents a name. 
/// The `id` field is first determined from the payload of the identifier token, meaning that it corresponds to the LexemeId.
/// However, once name resolution occurs, the `id` field is then reused to be the final value of the SymbolId in the symbol table.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var {
    id: SymbolId,
    span: Span
}

impl Var {
    pub fn new(id: SymbolId, span: Span) -> Self {
        Self { id, span }
    }

    pub fn with_zero_id(span: Span) -> Self {
        Self {
            id: 0,
            span
        }
    }

    pub fn from_token_payload(token: Token) -> Self {
        Self {
            id: token.payload() as usize,
            span: token.span()
        }
    }

    /// Creates a fake Token from the Var.
    pub fn synth_token(&self) -> Token {
        Token::with_payload(TokenKind::Ident, self.id as u32, self.span)
    }

    pub fn get_lexeme<'s>(&self, source_map: &'s SourceMap) -> &'s str {
        self.span.get_lexeme(source_map)
    }

    pub fn id(&self) -> SymbolId {
        self.id
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl TryFrom<Token> for Var {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.kind() {
            TokenKind::Ident => Ok(Var::from_token_payload(value)),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Oper {
    id: LexemeId,
    span: Span
}

impl Oper {
    pub fn new_unchecked(id: LexemeId, span: Span) -> Self {
        Self { id, span }
    }

    /// Checks if `id` is nonzero
    pub fn new(id: LexemeId, span: Span) -> Result<Self, ()> {
        if id == 0 {
            Err(())
        } else {
            Ok(Self::new_unchecked(id, span))
        }
    }

    pub fn from_token_payload_unchecked(token: Token) -> Self {
        Self {
            id: token.payload(),
            span: token.span()
        }
    }

    pub fn from_token_payload(token: Token) -> Result<Self, ()> {
        if token.payload() == 0 {
            Err(())
        } else {
            Ok(Self::from_token_payload_unchecked(token))
        }
    }

    /// Creates a fake Token from the Oper.
    pub fn synth_token(&self) -> Token {
        Token::with_payload(TokenKind::Operator, self.id, self.span)
    }

    pub fn get_lexeme<'s>(&self, source_map: &'s SourceMap) -> &'s str {
        self.span.get_lexeme(source_map)
    }

    pub fn id(&self) -> LexemeId {
        self.id
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl TryFrom<Token> for Oper {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value.kind() {
            TokenKind::Operator => Oper::from_token_payload(value),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
    bindings: Vec<Binding>,
    kind: LetKind,
    value: Option<Expr>,
}

impl Let {
    pub fn new(bindings: Vec<Binding>, kind: LetKind, value: Option<Expr>) -> Self {
        Self { bindings, kind, value }
    }

    pub fn bindings_mut(&mut self) -> &mut Vec<Binding> {
        &mut self.bindings
    }

    pub fn value_mut(&mut self) -> &mut Option<Expr> {
        &mut self.value
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LetKind {
    Assign, // assign symbols to value
    Define, // define symbols via constraint
    Declare // declare existence of symbols
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binding {
    Name(Var, Option<Type>),
    Fn(FnHeader),   // when it is known that it is a function binding
    Call(FnHeader), // when it is unknown whether it is a function or constructor binding
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    pub fn name_mut(&mut self) -> &mut Var {
        &mut self.name
    }

    pub fn ty_args_mut(&mut self) -> &mut Vec<Generic> {
        &mut self.ty_args
    }

    pub fn args_mut(&mut self) -> &mut Vec<(Var, Option<Type>)> {
        &mut self.args
    }

    pub fn kwargs_mut(&mut self) -> &mut Vec<(Var, Option<Type>)> {
        &mut self.kwargs
    }

    pub fn ty_mut(&mut self) -> &mut Option<Type> {
        &mut self.ty
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit {
        span: Span
    },
    Grouping {
        ty: Box<Type>,
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
    },
    Exponent {
        ty: Box<Type>,
        exp: Box<Expr>, // must be a Nat; notably must be an AstInt::Small as it is constrained by MAX_ARGS.
        span: Span
    }
    // more
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Unit { span } => *span,
            Type::Grouping { span, .. } => *span,
            Type::Named(var) => var.span,
            Type::Array { span, .. } => *span,
            Type::Tuple { span, .. } => *span,
            Type::Exponent { span, .. } => *span
        }
    }

    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Type::Unit { span } => &mut *span,
            Type::Grouping { span, .. } => &mut *span,
            Type::Named(var) => &mut var.span,
            Type::Array { span, .. } => &mut *span,
            Type::Tuple { span, .. } => &mut *span,
            Type::Exponent { span, .. } => &mut *span
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Shape {
    Empty,
    Dynamic,
    Specified(Vec<ShapeSpec>)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ShapeSpec {
    Known(Expr), // TODO: Perhaps split into KnownValue(Int) and KnownName(Var)
    Unknown
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Generic {
    pub name: Var,
    // pub sat: Option<Var>      // TODO: figure out if we are going to be doing a sat system or impl or whatnot
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variant {
    Const(Var),
    Tuple(Vec<Type>),
    Record(Vec<(Var, Type)>)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AliasLeft {
    Ident(Var),
    Oper(Oper)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AliasRight {
    Ident(Var),
    Oper(Oper),
    OpLit(OpLit),
    Expr(Expr)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Endpoint {
    Inclusive(Box<Expr>),
    Exclusive(Box<Expr>),
    Unspecified
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RangeStep {
    Discrete(Box<Expr>),
    Continuous
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operation {
    Ident(Var),
    Oper(Oper),
    OpLit(OpLit),
}

impl Operation {
    pub fn span(&self) -> Span {
        match self {
            Self::Ident(name) => name.span(),
            Self::Oper(op) => op.span(),
            Self::OpLit(oplit) => oplit.span()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OpLit {
    assoc: Assoc,
    name: Var,
    prec: Prec,
    span: Span,
}

impl OpLit {
    pub fn new(assoc: Assoc, name: Var, prec: Prec, span: Span) -> Self {
        Self { assoc, name, prec, span }
    }

    pub fn with_assoc(assoc: Assoc, name: Var, span: Span) -> Self {
        Self {
            assoc,
            name,
            prec: Prec::Call,
            span
        }
    }

    pub fn with_prec(name: Var, prec: Prec, span: Span) -> Self {
        Self {
            assoc: Assoc::None,
            name,
            prec,
            span
        }
    }
    
    pub fn with_name(name: Var, span: Span) -> Self {
        Self {
            assoc: Assoc::None,
            name,
            prec: Prec::Call,
            span
        }
    }

    pub fn assoc(&self) -> Assoc {
        self.assoc
    }

    pub fn name(&self) -> Var {
        self.name
    }

    pub fn name_mut(&mut self) -> &mut Var {
        &mut self.name
    }

    pub fn prec(&self) -> Prec {
        self.prec
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn get_lexeme<'s>(&self, source_map: &'s SourceMap) -> &'s str {
        self.span.get_lexeme(source_map)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Assoc {
    None,
    Left,
    Right
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Prec {
    Lowest,
    Assign,
    Lambda,
    Is,
    Or,
    Xor,
    And,
    Comparison,
    Range,
    Additive,
    Multiplicative,
    Exponentative,
    As,
    Composition,
    Unary,
    Call,
    Highest,
    Access,
    Group
}

impl Prec {
    pub const MIN_CUSTOM_PREC: Prec = Prec::Lowest;
    pub const MAX_CUSTOM_PREC: Prec = Prec::Highest;

    /// Gives binding power
    pub fn bp(self) -> u32 {
        (self as u32 + 1) * 10
    }
}

impl TryFrom<u32> for Prec {
    type Error = ();
    
    fn try_from(value: u32) -> Result<Self, Self::Error> {        
        match value {
            0 => Ok(Prec::Lowest),
            1 => Ok(Prec::Assign),
            2 => Ok(Prec::Lambda),
            3 => Ok(Prec::Is),
            4 => Ok(Prec::Or),
            5 => Ok(Prec::Xor),
            6 => Ok(Prec::And),
            7 => Ok(Prec::Comparison),
            8 => Ok(Prec::Range),
            9 => Ok(Prec::Additive),
            10 => Ok(Prec::Multiplicative),
            11 => Ok(Prec::Exponentative),
            12 => Ok(Prec::As),
            13 => Ok(Prec::Composition),
            14 => Ok(Prec::Unary),
            15 => Ok(Prec::Call),
            16 => Ok(Prec::Highest),
            17 => Ok(Prec::Access),
            18 => Ok(Prec::Group),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StringPart {
    Text(String),        // converts escape sequences
    Expr(Expr)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OperationItem {
    Expr(Box<Expr>),
    Ident(Var),
    Oper(Oper),
    OpLit(OpLit),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Wildcard(Span),
    Etc(Span),
    Rest {
        name: Var,
        span: Span
    },
    Unit(Span),
    Ident {
        name: Var,
        span: Span
    },
    Int {
        value: AstInt,
        span: Span
    },
    Pin {
        name: Var,
        span: Span
    },
    As {
        pat: Box<Pattern>,
        name: Var,
        span: Span
    },
    Type {
        ty: Type,
        span: Span
    },
    // path
    Tuple {
        pats: Vec<Pattern>,
        span: Span
    },
    Array {
        pats: Vec<Pattern>,
        span: Span
    },
    // more
    Or {
        lhs: Box<Pattern>,
        rhs: Box<Pattern>,
        span: Span
    },
    And {
        lhs: Box<Pattern>,
        rhs: Box<Pattern>,
        span: Span
    },
    Not {
        pat: Box<Pattern>,
        span: Span
    },
    Group {
        pat: Box<Pattern>,
        span: Span
    },
    // Sym(SymPattern)
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub enum SymPattern {
//     // <expr>
//     /* ?[x]a
//         binding         = '?' var_bindings? IDENT type_annot? ;
//         var_bindings    = '[' ( '$' IDENT )+ ']' ;
//         type_annot      = ':' type ;
//     */
//     // $x (with optional type annotation)
//     // ... <pat> 
//     // if
//     // when
//     ExprBind {   // ?[... $vars]
//         var_bindings: Vec<SymVar>,
//         name: Var,
//         ty: Option<Type>,
//         span: Span
//     },
//     VarBind(SymVar),
//     Rest {
//         pat: Box<SymPattern>,
//         span: Span
//     },
//     Guard {
//         cond: Box<Expr>,
//         span: Span
//     }
// }

// struct SymVar {
    
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatternGuard {
    guard: Expr,
    span: Span
}

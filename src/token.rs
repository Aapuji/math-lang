use crate::source::{SourceId, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident, // follows regex: [a-zA-Z\][a-zA-Z0-9_]*
    String,
    Int,
    Real,
    Complex,
    OperationString, // follows format `[1: OR 2:] <ident|slash-ident>`

    Comment,

    Operator, // any operator string

    // Separators
    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,

    Dot,
    Comma,
    Semicolon,

    At,
    Underscore,

    // Keywords
    Let, Var, Const, Sym, Alias, In,
    For, While, If, Else, When, Using,
    And, Or, Not, As,
    SlashIn,

    Error,
    
    EOF // has length 0
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    kind: TokenKind,
    span: Span
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn eof(i: usize, source_id: SourceId) -> Self {
        Self {
            kind: TokenKind::EOF,
            span: Span::new(i, i, source_id)
        }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn set_kind(&mut self, kind: TokenKind) {
        self.kind = kind;
    }

    pub fn set_span_start(&mut self, start: usize) {
        self.span.set_start(start);
    }
}

pub const OPERATOR_CHARSET: &'static str = "=:+-*/^%<>!&|~$?@\\";

/* Default Operators
    // 1 Char Operators
    Plus, Minus, Star, Slash, Caret, Percent,
    Bar, Amp, Bang, 
    Less, Greater, 
    Equal, Tilde, 
    Colon,

    // 2 Char Operators
    PlusMinus, MinusPlus, DoubleSlash,
    LessEq, GreaterEq, EqEq, BangEq, TildeEq,
    LessColon, GreaterColon, ColonLess,
    PlusEq, MinusEq, StarEq, SlashEq, CaretEq, BarEq, AmpEq,

    // 3+ Char Operators
    LessColonLess
*/

/* Operator Charset 

! % & * + - * 




*/

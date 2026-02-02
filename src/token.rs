use crate::source::{Source, SourceId, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident, // follows regex: [a-zA-Z\][a-zA-Z0-9_]*
    String,
    Int,
    Float,

    Operator, // any operator string

    // Separators
    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,

    Dot,
    Comma,
    Semicolon,

    // Keywords
    Let, Var, Const, Sym,
    For, While, If, Else, When, Using,
    And, Or, Not, As,

    EOF // has length _
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
}

/*
    // 1 Char Operators
    Plus, Minus, Star, Slash, Caret,
    Bar, Amp, Bang, 
    Less, Greater, 
    Equal, Tilde, 
    Colon,

    // 2 Char Operators
    PlusMinus, MinusPlus, DoubleSlash,
    LessEq, GreaterEq, EqEq, TildeEq,
    LessColon, GreaterColon, ColonLess,
    PlusEq, MinusEq, StarEq, SlashEq, CaretEq, BarEq, AmpEq,

    // 3+ Char Operators
    LessColonLess

*/


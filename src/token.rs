use crate::source::{SourceId, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,  // follows regex: [a-zA-Z\][a-zA-Z0-9_]*
    Int,
    Real,
    Sci,    // '<real>e<+|-><nat>' 
    Complex,

    // Strings
    StringStart, StringSegment, StringEnd,
    EscapeSeq, InterpolateStart, InterpolateEnd,

    DocComment,

    Operator, // any operator

    At,
    Backtick,
    Underscore,

    // Separators
    LParen, RParen,
    LBracket, RBracket,
    LBrace, RBrace,

    Dot,
    Comma,
    Semicolon,

    // Keywords
    Let, Var, Const, Fn, Sym, Enum, Struct, Alias,
    For, While, If, Else, Match, When, Using, In,
    And, Or, Not, As,
    SlashIn,

    Error(LexerErrorKind),
    
    EOF // has length 0
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerErrorKind {
    UnknownCharacter,
    UnterminatedString,
    UnterminatedBlockComment,
    InvalidEscapeSequence,
    OutOfRangeHexEscape,
    UnterminatedInterpolation
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

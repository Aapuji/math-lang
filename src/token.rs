use std::{collections::HashMap, hash::Hash};
use std::ops::RangeInclusive;

use lasso::{Key, Rodeo, RodeoResolver, Spur};

use crate::source::{SourceId, SourceMap, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Ident,  // follows regex [a-zA-Z_][a-zA-Z0-9_]* along with a valid \ string; a \ string can be `\<ident>` or `\<oper>`
    Int,
    Real,
    Sci,    // '<real>e[+|-]<nat>' 
    Imag,

    // Strings
    StringStart, StringSegment, StringEnd,
    EscapeSeq, InterpolateStart, InterpolateEnd,

    DocComment,

    Operator, // any operator
    CodeSpliceIndicator,
    
    Eq,
    ColonEq,

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

    // Keywords -- when changing this list, make sure to change the list in the symbol interner 
    Let, Var, Const, Fn, Sym, Context, Enum, Struct, Type,
    Macro, Alias,
    For, While, If, Else, Match, When, Using, In,
    And, Or, Xor, Not, Is, As,
    SlashIn, SlashNotIn,

    Error(LexerErrorKind),
    
    EOF // has length 0
}

impl TokenKind {
    pub fn is_keyword(&self) -> bool {
        use TokenKind::*;
        
        match self {
            Let        |
            Var        |
            Const      |
            Fn         |
            Sym        |
            Context    |
            Enum       |
            Struct     |
            Type       |
            Macro      |
            Alias      | 
            For        |
            While      |
            If         |
            Else       |
            Match      |
            When       |
            Using      |
            In         |
            And        |
            Or         |
            Xor        |
            Not        |
            As         |
            SlashIn    |
            SlashNotIn => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LexerErrorKind {
    UnknownCharacter,
    InvalidScientificLiteral,
    UnterminatedString,
    UnterminatedBlockComment,
    InvalidEscapeSequence,
    OutOfRangeHexEscape,
    UnterminatedInterpolation
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// The `payload` field is a generic payload. For tokens that do not use it, it does not add any extra memory usage.
///   - Identifiers and operators use it as an Option<Spur> as the index into the Rodeo string interning system
///   - perhaps in the future, optimizations can be made to put small integers or floats, etc into this to make it faster
pub struct Token {
    kind: TokenKind,
    payload: u32,       // A generic payload
    span: Span
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, payload: 0, span }
    }

    pub fn with_payload(kind: TokenKind, payload: u32, span: Span) -> Self {
        Self { kind, payload, span }
    }

    pub fn eof(i: usize, source_id: SourceId) -> Self {
        Self {
            kind: TokenKind::EOF,
            payload: 0,
            span: Span::new(i, i, source_id)
        }
    }

    pub fn can_be_operator(&self) -> bool {
        match self.kind {
            TokenKind::Operator   |
            TokenKind::Or         |
            TokenKind::Xor        |
            TokenKind::Not        |
            TokenKind::SlashIn    |
            TokenKind::SlashNotIn => true,
            _ => false
        }
    }

    pub fn is_builtin_operator(&self, interner: &ResolvedInterner) -> bool {
        // fine if sizeof(usize) >= sizeof(u32)
        if interner.is_builtin_operator(&Spur::try_from_usize(self.payload as usize).unwrap()) {
            // is operator
            self.kind == TokenKind::Operator ||
            // is both operator and keyword (like as, is, etc)
            interner.get_keyword(&Spur::try_from_usize(self.payload as usize).unwrap()).is_some()
        } else {
            false
        }
    }

    pub fn is_terminating(&self, source_map: &SourceMap) -> bool {
        match self.get_lexeme(source_map) {
            ")"  |
            "]"  |
            "}"  |
            ","  |
            ";"  |
            "in" => true,
            _ => false
        }
    }

    pub fn is_accessor(&self, source_map: &SourceMap) -> bool {
        match self.get_lexeme(source_map) {
            "."  |
            ".@" => true,
            _ => false
        }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn payload(&self) -> u32 {
        self.payload
    }

    pub fn set_kind(&mut self, kind: TokenKind) {
        self.kind = kind;
    }

    pub fn set_span_start(&mut self, start: usize) {
        self.span.set_start(start);
    }

    pub fn get_lexeme<'s>(&self, source_map: &'s SourceMap) -> &'s str {
        self.span.get_lexeme(source_map)
    }
}

pub const OPERATOR_CHARSET: &'static str = "=:+-*/^.%<>!&|~$?@";

#[derive(Debug, Clone)]
pub struct ActiveInterner {
    rodeo: Rodeo,
    keywords: HashMap<Spur, TokenKind>,
    builtin_operator_range: RangeInclusive<Spur>
}

impl ActiveInterner {
    pub fn new() -> Self {
        let mut rodeo = Rodeo::new();
        let mut keywords = HashMap::new();

        // keywords
        keywords.insert(rodeo.get_or_intern_static("let"), TokenKind::Let);
        keywords.insert(rodeo.get_or_intern_static("var"), TokenKind::Var);
        keywords.insert(rodeo.get_or_intern_static("const"), TokenKind::Const);
        keywords.insert(rodeo.get_or_intern_static("fn"), TokenKind::Fn);
        keywords.insert(rodeo.get_or_intern_static("sym"), TokenKind::Sym);
        keywords.insert(rodeo.get_or_intern_static("context"), TokenKind::Context);
        keywords.insert(rodeo.get_or_intern_static("enum"), TokenKind::Enum);
        keywords.insert(rodeo.get_or_intern_static("struct"), TokenKind::Struct);
        keywords.insert(rodeo.get_or_intern_static("type"), TokenKind::Type);
        keywords.insert(rodeo.get_or_intern_static("macro"), TokenKind::Macro);
        keywords.insert(rodeo.get_or_intern_static("alias"), TokenKind::Alias);
        keywords.insert(rodeo.get_or_intern_static("for"), TokenKind::For);
        keywords.insert(rodeo.get_or_intern_static("while"), TokenKind::While);
        keywords.insert(rodeo.get_or_intern_static("if"), TokenKind::If);
        keywords.insert(rodeo.get_or_intern_static("else"), TokenKind::Else);
        keywords.insert(rodeo.get_or_intern_static("match"), TokenKind::Match);
        keywords.insert(rodeo.get_or_intern_static("when"), TokenKind::When);
        keywords.insert(rodeo.get_or_intern_static("using"), TokenKind::Using);

        // both keywords and builtin operators
        let ostart = rodeo.get_or_intern_static("in");
        keywords.insert(ostart, TokenKind::In);
        keywords.insert(rodeo.get_or_intern_static("and"), TokenKind::And);
        keywords.insert(rodeo.get_or_intern_static("or"), TokenKind::Or);
        keywords.insert(rodeo.get_or_intern_static("xor"), TokenKind::Xor);
        keywords.insert(rodeo.get_or_intern_static("not"), TokenKind::Not);
        keywords.insert(rodeo.get_or_intern_static("is"), TokenKind::Is);
        keywords.insert(rodeo.get_or_intern_static("as"), TokenKind::As);
        keywords.insert(rodeo.get_or_intern_static(r"\in"), TokenKind::SlashIn);
        keywords.insert(rodeo.get_or_intern_static(r"\notIn"), TokenKind::SlashNotIn);

        // builtin operators
        rodeo.get_or_intern_static("∈");
        rodeo.get_or_intern_static("∉");
        rodeo.get_or_intern_static("+");
        rodeo.get_or_intern_static("+=");
        rodeo.get_or_intern_static("-");
        rodeo.get_or_intern_static("-=");
        rodeo.get_or_intern_static("+-");
        rodeo.get_or_intern_static("+-=");
        rodeo.get_or_intern_static("-+");
        rodeo.get_or_intern_static("-+=");
        rodeo.get_or_intern_static("*");
        rodeo.get_or_intern_static("*=");
        rodeo.get_or_intern_static("/");
        rodeo.get_or_intern_static("/=");
        rodeo.get_or_intern_static("//");
        rodeo.get_or_intern_static("//=");
        rodeo.get_or_intern_static("%");
        rodeo.get_or_intern_static("%=");
        rodeo.get_or_intern_static("%%");
        rodeo.get_or_intern_static("%%=");
        rodeo.get_or_intern_static("^");
        rodeo.get_or_intern_static("^=");
        rodeo.get_or_intern_static("|");
        rodeo.get_or_intern_static("let");
        rodeo.get_or_intern_static("==");
        rodeo.get_or_intern_static("!=");
        rodeo.get_or_intern_static("<");
        rodeo.get_or_intern_static("<=");
        rodeo.get_or_intern_static(">");
        rodeo.get_or_intern_static(">=");
        rodeo.get_or_intern_static("...");
        rodeo.get_or_intern_static("..");
        rodeo.get_or_intern_static("..+");
        rodeo.get_or_intern_static("..-");
        rodeo.get_or_intern_static("<..");
        rodeo.get_or_intern_static("<..+");
        rodeo.get_or_intern_static("<..-");
        rodeo.get_or_intern_static("..<");
        rodeo.get_or_intern_static("..<+");
        rodeo.get_or_intern_static("..<-");
        rodeo.get_or_intern_static("<..<");
        rodeo.get_or_intern_static("<..<+");
        rodeo.get_or_intern_static("<..<-");
        rodeo.get_or_intern_static(":");
        rodeo.get_or_intern_static(":+");
        rodeo.get_or_intern_static(":-");
        rodeo.get_or_intern_static("<:");
        rodeo.get_or_intern_static("<:+");
        rodeo.get_or_intern_static("<:-");
        rodeo.get_or_intern_static(":<");
        rodeo.get_or_intern_static(":<+");
        rodeo.get_or_intern_static(":<-");
        rodeo.get_or_intern_static("<:<");
        rodeo.get_or_intern_static("<:<+");
        rodeo.get_or_intern_static("<:<-");
        rodeo.get_or_intern_static("::");
        rodeo.get_or_intern_static("<::");
        rodeo.get_or_intern_static(":<:");
        rodeo.get_or_intern_static("->");
        let oend = rodeo.get_or_intern_static("=>");

        Self {
            rodeo,
            keywords,
            builtin_operator_range: ostart..=oend
        }
    }

    pub fn get_or_intern(&mut self, lexeme: &str) -> Spur {
        self.rodeo.get_or_intern(lexeme)
    }

    pub fn get_keyword(&self, spur: &Spur) -> Option<TokenKind> {
        self.keywords
            .get(spur)
            .copied()
    }

    pub fn is_builtin_operator(&self, spur: &Spur) -> bool {
        self.builtin_operator_range.contains(spur)
    }

    pub fn into_resolver(self) -> ResolvedInterner {
        ResolvedInterner {
            rodeo_resolver: self.rodeo.into_resolver(),
            keywords: self.keywords,
            builtin_operator_range: self.builtin_operator_range
        }
    }
}

#[derive(Debug)]
pub struct ResolvedInterner {
    rodeo_resolver: RodeoResolver,
    keywords: HashMap<Spur, TokenKind>,
    builtin_operator_range: RangeInclusive<Spur>
}

impl ResolvedInterner {
    // add wrapper methods for any methods of RodeoResolver as needed

    pub fn get_keyword(&self, spur: &Spur) -> Option<TokenKind> {
        self.keywords
            .get(spur)
            .copied()
    }

    pub fn is_builtin_operator(&self, spur: &Spur) -> bool {
        self.builtin_operator_range.contains(spur)
    }
}

pub type LexemeId = u32;

use std::iter::Peekable;
use std::str::CharIndices;
use std::sync::BarrierWaitResult;

use unicode_ident::{is_xid_start, is_xid_continue};

use crate::source::{SourceId, SourceMap, Span};
use crate::token::{LexerErrorKind, OPERATOR_CHARSET, Token, TokenKind};

#[derive(Debug)]
pub struct Lexer<'t> {
    text: Peekable<CharIndices<'t>>,
    len: usize,
    source: SourceId,
    ich: Option<(usize, char)>,
}

impl<'t> Lexer<'t> {
    pub fn new(content: &'t str, source: SourceId) -> Self {                
        Self {
            text: content.char_indices().peekable(),
            len: content.len(),
            source,
            ich: None,
        }
    }

    /// Consumes lexer to lex the entire source file.
    pub fn lex(mut self, source_map: &mut SourceMap) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];

        self.next();

        loop {
            if let Some((i, ch)) = self.ich {
                if ch.is_ascii_digit() {
                    self.lex_number((i, ch), &mut tokens);
                } else if ch == '"' {
                    self.lex_string((i, ch), &mut tokens, StringPrefix::None);
                } else if ch == 'f' && matches!(self.text.peek(), Some(&(_, '"'))) {
                    self.lex_string((i, ch), &mut tokens, StringPrefix::F);
                } else if ch == 'r' && matches!(self.text.peek(), Some(&(_, '"'))) {
                    self.lex_string((i, ch), &mut tokens, StringPrefix::R);
                } else if ch.is_whitespace() {
                    self.next();
                } else if ch == '#' {
                    self.lex_comment((i, ch), &mut tokens);
                } else if ch == '(' {
                    tokens.push(Token::new(
                        TokenKind::LParen, 
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == ')' {
                    tokens.push(Token::new(
                        TokenKind::RParen,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == '[' {
                    tokens.push(Token::new(
                        TokenKind::LBracket,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == ']' {
                    tokens.push(Token::new(
                        TokenKind::RBracket,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == '{' {
                    tokens.push(Token::new(
                        TokenKind::LBrace,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == '}' {
                    tokens.push(Token::new(
                        TokenKind::RBrace,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == '_' && !matches!(self.text.peek(), Some(&(_, c)) if is_xid_start(c) || c == '_') {
                    tokens.push(Token::new(
                        TokenKind::Underscore,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == 'i' && !matches!(self.text.peek(), Some(&(_, c)) if is_xid_start(c) || c == '_') {
                    tokens.push(Token::new(
                        TokenKind::Complex,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if is_xid_start(ch) || ch == '_' {
                    self.lex_ident(false, (i, ch), &mut tokens, source_map);
                } else if ch == '\\' {
                    match self.text.peek() {
                        Some(&(new_i, ch)) if OPERATOR_CHARSET.contains(ch) => {
                            let start = i;
                            self.next();
                            self.lex_operator((new_i, ch), &mut tokens);
                            
                            let token = tokens.last_mut().unwrap();
                            token.set_kind(TokenKind::Ident);
                            token.set_span_start(start);
                        }

                        Some(&(new_i, ch)) if ch.is_ascii_alphanumeric() || ch == '_' => {
                            let start = i;
                            self.next();
                            self.lex_ident(true, (new_i, ch), &mut tokens, source_map);

                            let token = tokens.last_mut().unwrap();
                            token.set_span_start(start);
                        }

                        Some(_) | None => {
                            tokens.push(Token::new(
                                TokenKind::Operator,
                                Span::new(i, i + 1, self.source)));
                            self.next();
                        }
                    }
                } else if ch == '@' && !matches!(self.text.peek(), Some(&(_, c)) if OPERATOR_CHARSET.contains(c)) {
                    tokens.push(Token::new(
                        TokenKind::At,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == '.' && !matches!(self.text.peek(), Some(&(_, c)) if OPERATOR_CHARSET.contains(c)) {
                    tokens.push(Token::new(
                        TokenKind::Dot,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == ',' {
                    tokens.push(Token::new(
                        TokenKind::Comma,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == ';' {
                    tokens.push(Token::new(
                        TokenKind::Semicolon,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if OPERATOR_CHARSET.contains(ch) {
                    self.lex_operator((i, ch), &mut tokens);
                } else if ch == '`' {
                    tokens.push(Token::new(
                        TokenKind::Backtick,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else {
                    tokens.push(Token::new(
                        TokenKind::Error(LexerErrorKind::UnknownCharacter),
                        Span::new(i, i + 1, self.source)));
                    self.next();
                }
            } else {
                tokens.push(Token::eof(self.len, self.source));
                break 
            }
        }
        
        return tokens;
    }

    fn lex_number(&mut self, ich: (usize, char), tokens: &mut Vec<Token>) {
        let start = ich.0;
        let mut end = start + 1;
        let mut kind = TokenKind::Int;

        loop {
            match self.next() {
                Some((i, ch)) if ch.is_ascii_digit() || ch == '_' => {
                    end = i + 1;
                }

                Some((i, ch)) if ch == '.' && kind == TokenKind::Int => {
                    match self.text.peek() {
                        // decimal
                        Some((i, ch)) if ch.is_ascii_digit() => {
                            end = i + 1;
                            self.next();
                            kind = TokenKind::Real;
                        }

                        _ => {
                            end = i;
                            break
                        }
                    }
                }

                Some((i, ch)) if ch == 'i' => {
                    end = i + 1;
                    kind = TokenKind::Complex;
                    self.next();
                    break
                }

                Some((i, _)) => {
                    end = i;
                    break
                }

                None => break
            }
        }

        let span = Span::new(start, end, self.source);
        tokens.push(Token::new(kind, span));
    }

    fn lex_string(&mut self, ich: (usize, char), tokens: &mut Vec<Token>, prefix: StringPrefix) {
        use StringPrefix::{None as N, F, R};
        
        let mut toks = vec![];
        let mut errs = vec![];

        let mut tok_kind = Some(TokenKind::StringStart);
        let mut tok_start = ich.0;
        let mut tok_end = ich.0 + if let N = prefix { 1 } else { self.next(); 2 };

        macro_rules! push_prev_tok {
            ( ) => {
                if let Some(kind) = tok_kind {
                    toks.push(Token::new(
                        kind, 
                        Span::new(tok_start, tok_end, self.source)));
                    
                    tok_kind = None;
                    tok_start = tok_end;
                }
            }
        };

        macro_rules! push_unterminated_str {
            ( $start:expr ) => {
                {
                    tokens.append(&mut errs);
                    tokens.push(Token::new(
                        TokenKind::Error(LexerErrorKind::UnterminatedString),
                        Span::new($start, self.len, self.source)));
                }
            };
        }

        loop {
            match (self.next(), prefix) {
                // end of string
                (Some((i, '"')), _) => {
                    push_prev_tok!();

                    tok_start = i;
                    tok_end = i + 1;

                    self.next();

                    if errs.is_empty() {
                        tokens.append(&mut toks);
                    } else {
                        return tokens.append(&mut errs);
                    }

                    return tokens.push(Token::new(
                        TokenKind::StringEnd,
                        Span::new(tok_start, tok_end, self.source)));
                }

                // escape sequences
                (Some((i, '\\')), N | F) => {
                    push_prev_tok!();

                    tok_kind = Some(TokenKind::EscapeSeq);
                    tok_start = i;
                    tok_end = i + 1;

                    match self.next() {
                        Some((i, 
                            '0' 
                            | '"' 
                            | '\\' 
                            | 'n' 
                            | 'r'
                            | 't'
                            | 'b' 
                            | 'f'
                            | 'v' 
                        )) => tok_end = i + 1,

                        Some((_, '\n')) => {
                            loop {
                                match self.text.peek() {
                                    Some(&(i, ch)) if ch.is_whitespace() => {
                                        tok_end = i + 1;
                                        self.next();
                                    }

                                    None => return push_unterminated_str!(ich.0),
                                    
                                    _ => break
                                }
                            }
                        }

                        Some((_, '\r')) if matches!(self.text.peek(), Some((_, '\n'))) => {
                            self.next();

                            loop {
                                match self.text.peek() {
                                    Some(&(i, ch)) if ch.is_whitespace() => {
                                        tok_end = i + 1;
                                        self.next();
                                    }

                                    None => return push_unterminated_str!(ich.0),
                                    
                                    _ => break
                                }
                            }
                        }

                        Some((_, 'x')) => {
                            let mut valid = true;

                            // 0-7
                            match self.text.peek() {
                                Some(&(i, ch)) if ('0'..='7').contains(&ch) => tok_end = i + 1,
                                
                                Some(&(i, ch)) if ch.is_ascii_hexdigit() => {
                                    errs.push(Token::new(
                                        TokenKind::Error(LexerErrorKind::OutOfRangeHexEscape),
                                        Span::new(tok_start, i + if self.text.peek().is_none() { 1 } else { 2 }, self.source)));

                                    tok_kind = None;
                                    valid = false;
                                }
                                
                                None => return push_unterminated_str!(ich.0),

                                Some(&(i, _)) => {
                                    errs.push(Token::new(
                                        TokenKind::Error(LexerErrorKind::InvalidEscapeSequence),
                                        Span::new(tok_start, i + 1, self.source)));
                                    
                                    tok_kind = None;
                                    valid = false;
                                }
                            }

                            if valid {
                                self.next();

                                // 0-F
                                match self.text.peek() {
                                    Some(&(i, ch)) if ch.is_ascii_hexdigit() => tok_end = i + 1,
                                    
                                    None => return push_unterminated_str!(ich.0),
                                    
                                    _ => {
                                        errs.push(Token::new(
                                            TokenKind::Error(LexerErrorKind::InvalidEscapeSequence),
                                            Span::new(tok_start, tok_end, self.source)));
                                        
                                        tok_kind = None;
                                        valid = false;
                                    }
                                }

                                if valid {
                                    self.next();
                                }
                            }
                        }

                        Some((_, 'u')) => {
                            for _ in 0..4 {
                                match self.text.peek() {
                                    Some(&(i, ch)) if ch.is_ascii_hexdigit() => tok_end = i + 1,

                                    None => return push_unterminated_str!(ich.0),

                                    _ => {
                                        errs.push(Token::new(
                                            TokenKind::Error(LexerErrorKind::InvalidEscapeSequence),
                                            Span::new(tok_start, tok_end, self.source)));

                                        tok_kind = None;    
                                        break
                                    }
                                }

                                self.next();
                            }
                        }

                        Some((_, 'U')) => {
                            for _ in 0..4 {
                                match self.text.peek() {
                                    Some(&(i, ch)) if ch.is_ascii_hexdigit() => tok_end = i + 1,

                                    None => return push_unterminated_str!(ich.0),

                                    _ => {
                                        errs.push(Token::new(
                                            TokenKind::Error(LexerErrorKind::InvalidEscapeSequence),
                                            Span::new(tok_start, tok_end, self.source)));
                                            
                                        tok_kind = None;
                                        break
                                    }
                                }

                                self.next();
                            }

                            for _ in 0..2 {
                                match self.text.peek() {
                                    Some((i, ch)) if ch.is_ascii_hexdigit() => tok_end = i + 1,

                                    None => return push_unterminated_str!(ich.0),

                                    _ => break
                                }

                                self.next();
                            }
                        }

                        None => return push_unterminated_str!(ich.0),

                        Some((i, _)) => {
                            errs.push(Token::new(
                                TokenKind::Error(LexerErrorKind::InvalidEscapeSequence),
                                Span::new(tok_start, i + 1, self.source)));
                            
                            tok_kind = None;
                        }
                    }
                }

                // any other character
                (Some((i, _)), _) => {
                    if let Some(true) = tok_kind.map(|kind| kind != TokenKind::StringSegment) {
                        push_prev_tok!();

                        tok_kind = Some(TokenKind::StringSegment);
                        tok_start = i;
                    }

                    tok_end = i + 1;
                }

                // eof
                (None, _) => return push_unterminated_str!(ich.0)
            }  
        }
    }

    // Note: Does not perform any normalization. But, that still must be done to make sure all identifiers are normalized accoridng to NFC (use unicode-normalization crate). Eg. e and aigu-mark should be normalized to e-aigu.
    fn lex_ident(&mut self, after_slash: bool, ich: (usize, char), tokens: &mut Vec<Token>, source_map: &mut SourceMap) {
        let start = ich.0;
        let mut end = start + 1;

        loop {
            match self.next() {
                Some((i, ch)) if is_xid_continue(ch) || ch == '_' => end = i + 1,
                Some((i, _)) => { end = i; break }
                None => break
            }
        }

        let span = Span::new(start, end, self.source);
        let text = source_map
            .get_source_mut(self.source)
            .data();

        if !after_slash {
            match &text[start..end] {
                "let" => tokens.push(Token::new(TokenKind::Let, span)),
                "var" => tokens.push(Token::new(TokenKind::Var, span)),
                "const" => tokens.push(Token::new(TokenKind::Const, span)),
                "fn" => tokens.push(Token::new(TokenKind::Fn, span)),
                "sym" => tokens.push(Token::new(TokenKind::Sym, span)),
                "alias" => tokens.push(Token::new(TokenKind::Alias, span)),
                "in" => tokens.push(Token::new(TokenKind::In, span)),
                "for" => tokens.push(Token::new(TokenKind::For, span)),
                "while" => tokens.push(Token::new(TokenKind::While, span)),
                "if" => tokens.push(Token::new(TokenKind::If, span)),
                "else" => tokens.push(Token::new(TokenKind::Else, span)),
                "match" => tokens.push(Token::new(TokenKind::Match, span)),
                "when" => tokens.push(Token::new(TokenKind::When, span)),
                "using" => tokens.push(Token::new(TokenKind::Using, span)),
                "and" => tokens.push(Token::new(TokenKind::And, span)),
                "or" => tokens.push(Token::new(TokenKind::Or, span)),
                "not" => tokens.push(Token::new(TokenKind::Not, span)),
                "as" => tokens.push(Token::new(TokenKind::As, span)),
                "∈" => tokens.push(Token::new(TokenKind::SlashIn, span)),
                _ => tokens.push(Token::new(TokenKind::Ident, span))
            }
        } else {
            match &text[start..end] {
                // TODO: Need to also lex some specially allowed math tokens, like ∈ as identifiers.
                "in" => tokens.push(Token::new(TokenKind::SlashIn, span)),
                _ => tokens.push(Token::new(TokenKind::Ident, span)),
            }
        }
    }

    fn lex_operator(&mut self, ich: (usize, char), tokens: &mut Vec<Token>) {
        let start = ich.0;
        let mut end = start + 1;

        loop {
            match self.next() {
                Some((i, ch)) if OPERATOR_CHARSET.contains(ch) => end = i + 1,
                Some((i, _)) => { end = i; break }
                None => break
            }
        }

        tokens.push(Token::new(
            TokenKind::Operator,
            Span::new(start, end, self.source)));
    }

    /// Skips regular comments (line: #, block: #= =#) but lexes doc comments (line: ##, block: ##= =#).
    fn lex_comment(&mut self, ich: (usize, char), tokens: &mut Vec<Token>) {
        let start = ich.0;

        match self.next() {
            // Block Comment
            Some((_, '=')) => {
                let mut comment_depth = 1;

                loop {
                    match self.next() {
                        Some((_, '=')) => if let Some(&(_, '#')) = self.text.peek() {
                            comment_depth -= 1;
                            self.next();
                            self.next();

                            if comment_depth == 0 {
                                break
                            }
                        }

                        Some((_, '#')) => if let Some(&(_, '=')) = self.text.peek() {
                            comment_depth += 1;
                            self.next();
                        } else if let Some((_, '#')) = self.next() {                            
                            if let Some((_, '=')) = self.text.peek() {
                                comment_depth += 1;
                                self.next();
                            }
                        }

                        Some(_) => (),

                        None => {
                            tokens.push(Token::new(
                            TokenKind::Error(LexerErrorKind::UnterminatedBlockComment),
                            Span::new(start, self.len, self.source)));
                            break;
                        }
                    }
                }
            }

            // Doc Comments
            Some((_, '#')) => if let Some(&(_, '=')) = self.text.peek() {
                let mut comment_depth = 1;

                loop {
                    match self.next() {
                        Some((_, '=')) => if let Some(&(i, '#')) = self.text.peek() {
                            comment_depth -= 1;
                            self.next();
                            self.next();

                            if comment_depth == 0 {
                                tokens.push(Token::new(
                                    TokenKind::DocComment,
                                    Span::new(start, i + 1, self.source)));
                                break
                            }
                        }

                        Some((_, '#')) => if let Some(&(_, '=')) = self.text.peek() {
                            comment_depth += 1;
                            self.next();
                        } else if let Some((_, '#')) = self.next() {                            
                            if let Some((_, '=')) = self.text.peek() {
                                comment_depth += 1;
                                self.next();
                            }
                        }

                        Some(_) => (),

                        None => {
                            tokens.push(Token::new(
                            TokenKind::Error(LexerErrorKind::UnterminatedBlockComment),
                            Span::new(start, self.len, self.source)));
                            break;
                        }
                    }
                }
            } else {
                loop {
                    match self.next() {
                        Some((i, '\n')) => {
                            tokens.push(Token::new(
                                TokenKind::DocComment,
                                Span::new(start, i + 1, self.source)));
                            self.next();
                            break
                        }

                        Some(_) => (),

                        None => {
                            tokens.push(Token::new(
                                TokenKind::DocComment,
                                Span::new(start, self.len, self.source)));
                            break
                        }
                    }
                }
            }

            // Line Comment
            _ => loop {
                match self.next() {
                    Some((_, '\n')) => {
                        self.next();
                        break
                    }

                    Some(_) => (),

                    None => break
                }
            }
        }
    }

    fn next(&mut self) -> Option<(usize, char)> {
        self.ich = self.text.next();
        self.ich
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringPrefix {
    None,
    F,
    R
}

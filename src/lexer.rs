use std::iter::Peekable;
use std::str::CharIndices;

use unicode_ident::{is_xid_start, is_xid_continue};

use crate::source::{SourceId, SourceMap, Span};
use crate::token::{LexerErrorKind, OPERATOR_CHARSET, Token, TokenKind};

#[derive(Debug)]
pub struct Lexer<'t> {
    text: Peekable<CharIndices<'t>>,
    len: usize,
    source: SourceId,
    ich: Option<(usize, char)>,
    mode_stack: Vec<LexerMode>          // empty means Normal mode  
}

impl<'t> Lexer<'t> {
    pub fn new(content: &'t str, source: SourceId) -> Self {                
        Self {
            text: content.char_indices().peekable(),
            len: content.len(),
            source,
            ich: None,
            mode_stack: vec![]
        }
    }

    /// Consumes lexer to lex the entire source file.
    pub fn lex(mut self, source_map: &mut SourceMap) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];

        self.next();

        loop {
            if let Some(&LexerMode::String(str_start, prefix)) = self.mode_stack.last() {
                self.continue_string(str_start, &mut tokens, prefix);
            } else if let Some((i, ch)) = self.ich {
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
                    if let Some(LexerMode::Interpolate(_, n, _)) = self.mode_stack.last_mut() {
                        *n += 1;
                    }

                    tokens.push(Token::new(
                        TokenKind::LBrace,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == '}' {
                    match self.mode_stack.last_mut() {
                        Some(LexerMode::Interpolate(_, 0, _)) => {
                            self.mode_stack.pop();

                            tokens.push(Token::new(
                                TokenKind::InterpolateEnd,
                                Span::new(i, i + 1, self.source)));
                        }

                        Some(LexerMode::Interpolate(_, n, _)) => {
                            *n -= 1;

                            tokens.push(Token::new(
                                TokenKind::RBrace,
                                Span::new(i, i + 1, self.source)));
                        }

                        _ => tokens.push(Token::new(
                            TokenKind::RBrace,
                            Span::new(i, i + 1, self.source)))
                    }

                    self.next();
                } else if ch == '_' && !matches!(self.text.peek(), Some(&(_, c)) if is_xid_start(c) || c == '_') {
                    tokens.push(Token::new(
                        TokenKind::Underscore,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch == 'i' && !matches!(self.text.peek(), Some(&(_, c)) if is_xid_start(c) || c == '_') {
                    tokens.push(Token::new(
                        TokenKind::Imag,
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
                while !self.mode_stack.is_empty() {
                    match self.mode_stack.last() {
                        Some(LexerMode::Interpolate(int_start, _, _)) => tokens.push(Token::new(
                            TokenKind::Error(LexerErrorKind::UnterminatedInterpolation),
                            Span::new(*int_start, self.len, self.source))),

                        Some(LexerMode::String(str_start, _)) => tokens.push(Token::new(
                            TokenKind::Error(LexerErrorKind::UnterminatedString),
                            Span::new(*str_start, self.len, self.source))),
                        
                        None => ()
                    }

                    self.mode_stack.pop();
                }

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

                Some((i, '.')) if kind == TokenKind::Int => {
                    match self.text.peek() {
                        // decimal
                        Some((i, ch)) if ch.is_ascii_digit() => {
                            end = i + 1;
                            kind = TokenKind::Real;
                            self.next();
                        }

                        _ => {
                            end = i;
                            break
                        }
                    }
                }

                Some((i, 'i')) => {
                    end = i + 1;
                    kind = TokenKind::Imag;
                    self.next();
                    break
                }

                Some((i, 'e' | 'E')) => {
                    end = i + 1;
                    kind = TokenKind::Sci;

                    match self.next() {
                        Some((i, '+' | '-')) => {
                            end = i + 1;

                            match self.next() {
                                Some((i, ch)) if ch.is_ascii_digit() => {
                                    end = i + 1;
                                }

                                _ => return tokens.push(Token::new(
                                    TokenKind::Error(LexerErrorKind::InvalidScientificLiteral),
                                    Span::new(start, end, self.source)))
                            }
                        }

                        Some((_, ch)) if ch.is_ascii_digit() => (),

                        _ => return tokens.push(Token::new(
                            TokenKind::Error(LexerErrorKind::InvalidScientificLiteral),
                            Span::new(start, end, self.source))),
                    }
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
        let start = ich.0;
        let end = ich.0 + if let StringPrefix::None = prefix { 1 } else { self.next(); 2 };
        
        tokens.push(Token::new(
            TokenKind::StringStart,
            Span::new(start, end, self.source)));
        
        self.mode_stack.push(LexerMode::String(ich.0, prefix));
        self.next();
        self.continue_string(ich.0, tokens, prefix);
    }

    fn continue_string(&mut self, str_start: usize, tokens: &mut Vec<Token>, prefix: StringPrefix) {
        use StringPrefix::{None as N, F};
        
        let mut kind = None;
        let mut start = str_start;
        let mut end = str_start + 1; // these will be reinitialized

        macro_rules! push_prev_tok {
            ( ) => {
                if let Some(k) = kind {
                    tokens.push(Token::new(k, Span::new(start, end, self.source)));
                    
                    kind = None;
                    start = end;
                }
            }
        }

        macro_rules! push_unterminated_str {
            ( ) => {
                {
                    while !self.mode_stack.is_empty() {
                        match self.mode_stack.last() {
                            Some(LexerMode::String(str_start, _)) => {
                                tokens.push(Token::new(
                                    TokenKind::Error(LexerErrorKind::UnterminatedString),
                                    Span::new(*str_start, self.len, self.source)
                                ));
                                self.mode_stack.pop();
                            } 

                            Some(LexerMode::Interpolate(int_start, _, _)) => {
                                tokens.push(Token::new(

                                    TokenKind::Error(LexerErrorKind::UnterminatedInterpolation),
                                    Span::new(*int_start, self.len, self.source)
                                ));
                            }

                            None => ()
                        }
                    }
                }
            };
        }

        loop {
            match (self.ich, prefix) {
                // end of string
                (Some((i, '"')), _) => {
                    push_prev_tok!();

                    start = i;
                    end = i + 1;

                    self.next();

                    self.mode_stack.pop();
                    return tokens.push(Token::new(
                        TokenKind::StringEnd,
                        Span::new(start, end, self.source)));
                }

                // fstring interpolation
                (Some((i, '{')), F) => {
                    if let Some(&(i, '{')) = self.text.peek() {
                        if let Some(TokenKind::StringSegment) = kind {
                            end = i + 1;
                        } else {
                            push_prev_tok!();

                            kind = Some(TokenKind::StringSegment);
                            start = i - 1;
                            end = i + 1;
                        }

                        self.next();
                    } else { // todo: format specifiers (width, etc.)?
                        push_prev_tok!();

                        kind = Some(TokenKind::InterpolateStart);
                        start = i;
                        end = i + 1;

                        push_prev_tok!();

                        self.mode_stack.push(LexerMode::Interpolate(i, 0, str_start));
                        self.next();
                        return;
                    }
                }

                // escape sequences
                (Some((i, '\\')), N | F) => {
                    push_prev_tok!();

                    kind = Some(TokenKind::EscapeSeq);
                    start = i;
                    end = i + 1;

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
                        )) => end = i + 1,

                        Some((_, '\n')) => {
                            loop {
                                match self.text.peek() {
                                    Some(&(i, ch)) if ch.is_whitespace() => {
                                        end = i + 1;
                                        self.next();
                                    }

                                    None => return push_unterminated_str!(),
                                    
                                    _ => break
                                }
                            }
                        }

                        Some((_, '\r')) if matches!(self.text.peek(), Some((_, '\n'))) => {
                            self.next();

                            loop {
                                match self.text.peek() {
                                    Some(&(i, ch)) if ch.is_whitespace() => {
                                        end = i + 1;
                                        self.next();
                                    }

                                    None => return push_unterminated_str!(),
                                    
                                    _ => break
                                }
                            }
                        }

                        // \xHH, HH <= 7F
                        Some((_, 'x')) => {
                            let mut valid = true;

                            // 0-7
                            match self.text.peek() {
                                Some(&(i, ch)) if ('0'..='7').contains(&ch) => end = i + 1,
                                
                                Some(&(i, ch)) if ch.is_ascii_hexdigit() => {
                                    tokens.push(Token::new(
                                        TokenKind::Error(LexerErrorKind::OutOfRangeHexEscape),
                                        Span::new(start, i + if self.text.peek().is_none() { 1 } else { 2 }, self.source)));

                                    kind = None;
                                    valid = false;
                                }
                                
                                None => return push_unterminated_str!(),

                                Some(&(i, _)) => {
                                    tokens.push(Token::new(
                                        TokenKind::Error(LexerErrorKind::InvalidEscapeSequence),
                                        Span::new(start, i + 1, self.source)));
                                    
                                    kind = None;
                                    valid = false;
                                }
                            }

                            if valid {
                                self.next();

                                // 0-F
                                match self.text.peek() {
                                    Some(&(i, ch)) if ch.is_ascii_hexdigit() => end = i + 1,
                                    
                                    None => return push_unterminated_str!(),
                                    
                                    _ => {
                                        tokens.push(Token::new(
                                            TokenKind::Error(LexerErrorKind::InvalidEscapeSequence),
                                            Span::new(start, end, self.source)));
                                        
                                        kind = None;
                                        valid = false;
                                    }
                                }

                                if valid {
                                    self.next();
                                }
                            }
                        }

                        // \uHHHH
                        Some((_, 'u')) => {
                            for _ in 0..4 {
                                match self.text.peek() {
                                    Some(&(i, ch)) if ch.is_ascii_hexdigit() => end = i + 1,

                                    None => return push_unterminated_str!(),

                                    _ => {
                                        tokens.push(Token::new(
                                            TokenKind::Error(LexerErrorKind::InvalidEscapeSequence),
                                            Span::new(start, end, self.source)));

                                        kind = None;    
                                        break
                                    }
                                }

                                self.next();
                            }
                        }

                        // \UHHHHHH
                        Some((_, 'U')) => {
                            for _ in 0..6 {
                                match self.text.peek() {
                                    Some(&(i, ch)) if ch.is_ascii_hexdigit() => end = i + 1,

                                    None => return push_unterminated_str!(),

                                    _ => {
                                        tokens.push(Token::new(
                                            TokenKind::Error(LexerErrorKind::InvalidEscapeSequence),
                                            Span::new(start, end, self.source)));
                                            
                                        kind = None;
                                        break
                                    }
                                }

                                self.next();
                            }
                        }

                        None => return push_unterminated_str!(),

                        Some((i, _)) => {
                            tokens.push(Token::new(
                                TokenKind::Error(LexerErrorKind::InvalidEscapeSequence),
                                Span::new(start, i + 1, self.source)));
                            
                            kind = None;
                        }
                    }
                }

                // any other character
                (Some((i, _)), _) => {
                    if let Some(true) | None = kind.map(|k| k != TokenKind::StringSegment) {
                        push_prev_tok!();

                        kind = Some(TokenKind::StringSegment);
                        start = i;
                    }

                    end = i + 1;
                }

                // eof
                (None, _) => return push_unterminated_str!()
            } 

            self.next(); 
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
                "enum" => tokens.push(Token::new(TokenKind::Enum, span)),
                "struct" => tokens.push(Token::new(TokenKind::Struct, span)),
                "alias" => tokens.push(Token::new(TokenKind::Alias, span)),
                "for" => tokens.push(Token::new(TokenKind::For, span)),
                "while" => tokens.push(Token::new(TokenKind::While, span)),
                "if" => tokens.push(Token::new(TokenKind::If, span)),
                "else" => tokens.push(Token::new(TokenKind::Else, span)),
                "match" => tokens.push(Token::new(TokenKind::Match, span)),
                "when" => tokens.push(Token::new(TokenKind::When, span)),
                "using" => tokens.push(Token::new(TokenKind::Using, span)),
                "in" => tokens.push(Token::new(TokenKind::In, span)),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexerMode {
    String(usize, StringPrefix),
    Interpolate(usize, i32, usize) // (int_start, bracket_depth, parent string's str_start)
}

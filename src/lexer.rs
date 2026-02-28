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
                    self.lex_string((i, ch), &mut tokens);
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

    
    fn lex_string(&mut self, ich: (usize, char), tokens: &mut Vec<Token>) {
        let start = ich.0;
        let end: usize;

        loop {
            match self.next() {
                Some((i, ch)) if ch == '"' => {
                    end = i + 1;
                    self.next();
                    break
                }

                Some(_) => {}

                None => {
                    tokens.push(Token::new(
                        TokenKind::Error(LexerErrorKind::UnterminatedString),
                        Span::new(start, self.len, self.source)));
                }
            }
        }

        let span = Span::new(start, end, self.source);
        tokens.push(Token::new(TokenKind::String, span));
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
                "when" => tokens.push(Token::new(TokenKind::When, span)),
                "using" => tokens.push(Token::new(TokenKind::Using, span)),
                "and" => tokens.push(Token::new(TokenKind::And, span)),
                "or" => tokens.push(Token::new(TokenKind::Or, span)),
                "not" => tokens.push(Token::new(TokenKind::Not, span)),
                "as" => tokens.push(Token::new(TokenKind::As, span)),
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

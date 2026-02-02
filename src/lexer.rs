use std::iter::Peekable;
use std::str::CharIndices;

use crate::source::{SourceId, Span};
use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub struct Lexer<'t> {
    text: Peekable<CharIndices<'t>>,
    source: SourceId,
    ich: Option<(usize, char)>
}

impl<'t> Lexer<'t> {
    pub fn new(content: &'t str, source: SourceId) -> Self {
        Self {
            text: content.char_indices().peekable(),
            source,
            ich: None
        }
    }

    /// Consumes lexer to lex the entire source file.
    pub fn lex(mut self) -> Vec<Token> {
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
                } else if ch == '_' && !matches!(self.text.peek(), Some(&(_, c)) if c.is_ascii_alphabetic() || c == '_') {
                    tokens.push(Token::new(
                        TokenKind::Underscore,
                        Span::new(i, i + 1, self.source)));
                    self.next();
                } else if ch.is_ascii_alphabetic() || ch == '_' {
                    self.lex_ident((i, ch), &mut tokens);
                }
            } else {
                tokens.push(Token::eof(0 /* todo */, self.source));
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
                            kind = TokenKind::Float;
                        }

                        _ => {
                            end = i;
                            break
                        }
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
                    todo!("unterminated string")
                }
            }
        }

        let span = Span::new(start, end, self.source);
        tokens.push(Token::new(TokenKind::String, span));
    }

    fn lex_ident(&mut self, ich: (usize, char), tokens: &mut Vec<Token>) {
        let start = ich.0;
        let mut end = start + 1;

        loop {
            match self.next() {
                Some((i, ch)) if ch.is_ascii_alphanumeric() || ch == '_' => end = i + 1,
                Some((i, _)) => { end = i; break }
                None => break
            }
        }

        tokens.push(Token::new(
            TokenKind::Ident, 
            Span::new(start, end, self.source)));
    }

    fn next(&mut self) -> Option<(usize, char)> {
        self.ich = self.text.next();
        self.ich
    }
}

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
                }
            } else {
                tokens.push(Token::eof(0 /* todo!("EOF character index") */, self.source));
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
        let mut end = start + 1;

        loop {
            match self.next() {
                Some((i, ch)) if ch == '"' => {
                    end = i + 1;
                    self.next();
                    break
                }

                Some((i, ch)) => {
                    end = i + 1;
                }

                None => {
                    todo!("unterminated string")
                }
            }
        }

        let span = Span::new(start, end, self.source);
        tokens.push(Token::new(TokenKind::String, span));
    }

    fn next(&mut self) -> Option<(usize, char)> {
        self.ich = self.text.next();
        self.ich
    }
}

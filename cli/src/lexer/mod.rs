use crate::error::position::Position;
use crate::lexer::source::Source;
use crate::lexer::tokens::{Token, TokenKind};
use anyhow::Result;
use crate::error::invalid_token;
use crate::error::span::TextSpan;
use crate::lexer::identifiers::Identifier;
use crate::lexer::numbers::NumberLiteral;
use crate::lexer::punctuation::Punctuation;
use crate::lexer::string::StringLiteral;

pub mod source;
mod tokens;
mod string;
mod numbers;
mod identifiers;
mod punctuation;

#[derive(Debug)]
pub struct Lexer {
    pub position: Position,
    pub tokens: Vec<Token>,
    pub source: Source,
}

impl Lexer {
    pub fn new(source: Source) -> Self {
        Self {
            position: Position::new(1, 0, 0),
            tokens: Vec::new(),
            source,
        }
    }

    pub fn lex(&mut self) -> Result<()> {
        loop {
            let token = self.next_token()?;

            if let Some(token) = token {
                if (token.kind == TokenKind::Comment)
                    || token.kind == TokenKind::Whitespace
                {
                    continue;
                }

                if token.kind == TokenKind::EOF {
                    break;
                }

                self.tokens.push(token);
            } else {
                break;
            }
        }

        Ok(())
    }

    pub fn current(&self) -> Option<char> {
        self.source.content.get_char(self.position.index)
    }

    pub fn consume(&mut self) -> Option<char> {
        if self.position.index >= self.source.content.len_chars() {
            return None;
        }

        let c = self.current();
        self.update_position(c?);

        c
    }

    pub fn update_position(&mut self, c: char) {
        if c == '\n' {
            self.position.line += 1;
            self.position.column = 0;
        } else {
            self.position.column += 1;
        }
        self.position.index += 1;
    }

    pub fn parse_char(&mut self) -> Result<char> {
        self.consume();
        let c = self.consume();
        if self.consume() == Some('\'') {
            Ok(c.unwrap())
        } else {
            Err(invalid_token(
                c.unwrap().to_string(),
                TextSpan::new(self.position, self.position, c.unwrap().to_string()),
            )
                .into())
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>> {
        let start = self.position;

        let Some(c) = self.current() else {
            return Ok(None);
        };

        let kind = match c {
            _ if c.is_whitespace() => {
                while let Some(c) = self.current() {
                    if !c.is_whitespace() {
                        break;
                    }
                    self.consume();
                }
                TokenKind::Whitespace
            }

            _ if c == '"' => StringLiteral::lex_string(self)?,
            _ if c.is_ascii_digit() => NumberLiteral::lex_number(self, c)?,
            _ if c == '\'' => TokenKind::Char(self.parse_char()?),

            _ if Identifier::is_identifier_start(c) => Identifier::lex_identifier(self)?,

            _ => Punctuation::lex_punctuation(self, c)?,
        };

        let end_pos = self.position;
        let literal = self.source.get_between(start.index, end_pos.index);
        Ok(Some(Token::new(
            kind,
            TextSpan::new(start, end_pos, literal),
        )))
    }

    pub fn lex_potential_double(
        &mut self,
        expected: char,
        one_char: TokenKind,
        double_char: TokenKind,
    ) -> TokenKind {
        if let Some(next) = self.peek() {
            if next == expected {
                self.consume();
                double_char
            } else {
                one_char
            }
        } else {
            one_char
        }
    }
    
    pub fn peek(&self) -> Option<char> {
        self.source.content.get_char(self.position.index + 1)
    }

    pub fn lex_potential_triple(
        &mut self,
        expected: char,
        one_char: TokenKind,
        double_char: TokenKind,
        triple_char: TokenKind,
    ) -> TokenKind {
        match self.peek() {
            Some(next) if next == expected => {
                self.consume();
                match self.peek() {
                    Some(next) if next == expected => {
                        self.consume();
                        triple_char
                    }
                    _ => double_char,
                }
            }
            _ => one_char,
        }
    }

    /// Check if the next character matches the given character.
    pub fn match_next(&mut self, ch: char) -> bool {
        if let Some(c) = self.peek() {
            if c == ch {
                return true;
            }
        }
        false
    }
}
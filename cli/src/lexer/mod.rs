use crate::error::position::Position;
use crate::lexer::source::Source;
use crate::lexer::tokens::{Token, TokenKind};
use anyhow::Result;
use crate::error::span::TextSpan;

pub mod source;
mod tokens;

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

    pub fn next_token(&mut self) -> Result<Option<Token>> {
        let start = self.position;

        let Some(c) = self.current() else {
            return Ok(None);
        };

        let end_pos = self.position;
        let literal = self.source.get_between(start.index, end_pos.index);
        Ok(Some(Token::new(
            TokenKind::Bad,
            TextSpan::new(start, end_pos, literal),
        )))
    }
}
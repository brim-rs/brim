use std::mem;
use crate::lexer::Lexer;
use anyhow::Result;
use brim::{
    compiler::CompilerContext, cursor::Cursor,
    files::SimpleFile,
    token::{Token, TokenKind},
    PrimitiveToken,
    PrimitiveTokenKind,
};
use tracing::debug;
use brim::span::Span;
use crate::parser::barrel::Barrel;
use crate::parser::cursor::TokenCursor;

pub mod barrel;
mod items;
mod cursor;

#[derive(Debug)]
pub struct Parser<'a> {
    pub file: &'a SimpleFile,
    pub cursor: Cursor<'a>,
    pub primitives: Vec<PrimitiveToken>,
    /// Used for documentation generation to keep comments in the AST. Only in `brim doc`.
    pub keep_comments: bool,
    pub tokens: Vec<Token>,
    pub token_cursor: TokenCursor,
    pub current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SimpleFile) -> Self {
        let cursor = Cursor::new(file.source());

        Self {
            file,
            cursor,
            primitives: vec![],
            keep_comments: false,
            tokens: vec![],
            token_cursor: TokenCursor::new(vec![]),
            current_token: Token::new(TokenKind::Skipable, Span::DUMMY),
        }
    }

    pub fn parse_barrel(&mut self, comp: &mut CompilerContext) -> Result<Barrel> {
        debug!("Lexing primitive tokens for a barrel");
        loop {
            let token = self.cursor.next_token();
            self.primitives.push(token.clone());

            if token.kind == PrimitiveTokenKind::Eof {
                break;
            }
        }

        let mut lexer = Lexer::new(self.file, &mut self.primitives);
        let mut tokens = vec![];

        while let Some(token) = lexer.next_token(comp) {
            if token.kind == TokenKind::Eof {
                break;
            }

            if token.kind == TokenKind::Skipable {
                continue;
            }

            tokens.push(token);
        }
        self.tokens = tokens;

        self.token_cursor = TokenCursor::new(self.tokens.clone());
        let mut items = vec![];
        
        // Advance to get the first token
        self.advance();
        
        loop {
            let Some(token) = self.parse_item()? else {
                break;
            };

            items.push(token);
        }

        Ok(Barrel {})
    }

    pub fn advance(&mut self) -> Token {
        self.token_cursor.bump();

        if let Some(token) = self.token_cursor.current() {
            self.current_token = token.clone();
        } else {
            self.current_token = Token::new(TokenKind::Eof, Span::DUMMY);
        }

        self.current_token.clone()
    }
}

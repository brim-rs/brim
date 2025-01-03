use crate::lexer::Lexer;
use anyhow::Result;
use brim::{
    PrimitiveToken, PrimitiveTokenKind, cursor::Cursor, files::SimpleFile, session::Session,
    token::TokenKind,
};
use tracing::debug;

#[derive(Debug)]
pub struct Parser<'a> {
    pub file: &'a SimpleFile,
    pub cursor: Cursor<'a>,
    pub primitives: Vec<PrimitiveToken>,
    /// Used for documentation generation to keep comments in the AST. Only in `brim doc`.
    pub keep_comments: bool,
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SimpleFile) -> Self {
        let cursor = Cursor::new(file.source());

        Self {
            file,
            cursor,
            primitives: vec![],
            keep_comments: false,
        }
    }

    pub fn parse_barrel(&mut self, sess: &mut Session) -> Result<()> {
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

        while let Some(token) = lexer.next_token(sess) {
            if token.kind == TokenKind::Eof {
                break;
            }

            if token.kind == TokenKind::Skipable {
                continue;
            }

            tokens.push(token);
        }

        for token in tokens {
            println!("{:?}", token);
        }

        Ok(())
    }
}

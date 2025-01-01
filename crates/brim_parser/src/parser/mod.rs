use brim::files::SimpleFile;
use brim::session::Session;
use anyhow::Result;
use tracing::debug;
use brim::cursor::Cursor;
use brim::{PrimitiveToken, PrimitiveTokenKind};
use brim::symbol::{intern, resolve, GLOBAL_INTERNER};
use crate::lexer::Lexer;

#[derive(Debug)]
pub struct Parser<'a> {
    pub sess: &'a Session<'a>,
    pub file: &'a SimpleFile,
    pub cursor: Cursor<'a>,
    pub primitives: Vec<PrimitiveToken>,
    /// Used for documentation generation to keep comments in the AST. Only in `brim doc`.
    pub keep_comments: bool,
}

impl<'a> Parser<'a> {
    pub fn new(sess: &'a Session<'a>, file: &'a SimpleFile) -> Self {
        let cursor = Cursor::new(file.source());

        Self { sess, file, cursor, primitives: vec![], keep_comments: false }
    }

    pub fn parse_barrel(&mut self) -> Result<()> {
        debug!("Lexing primitive tokens for a barrel");
        loop {
            let token = self.cursor.next_token();
            self.primitives.push(token.clone());

            if token.kind == PrimitiveTokenKind::Eof {
                break;
            }
        }

        let mut lexer = Lexer::new(self.sess, self.file, &mut self.primitives);
        let mut tokens = vec![];

        while let Some(token) = lexer.next_token() {
            if token.kind == PrimitiveTokenKind::Eof {
                break;
            }

            if token.kind == PrimitiveTokenKind::Whitespace ||
                matches!(token.kind, PrimitiveTokenKind::Comment { doc: true }) {
                continue;
            }

            tokens.push(token);
        }

        Ok(())
    }
}
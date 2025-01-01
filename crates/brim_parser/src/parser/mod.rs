use brim::files::SimpleFile;
use brim::session::Session;
use anyhow::Result;
use tracing::debug;
use brim::cursor::Cursor;
use brim::{PrimitiveToken, PrimitiveTokenKind};

#[derive(Debug)]
pub struct Parser<'a> {
    pub sess: &'a Session<'a>,
    pub file: &'a SimpleFile,
    pub cursor: Cursor<'a>,
    pub primitives: Vec<PrimitiveToken>,
}

impl<'a> Parser<'a> {
    pub fn new(sess: &'a Session<'a>, file: &'a SimpleFile) -> Self {
        let cursor = Cursor::new(file.source());

        Self { sess, file, cursor, primitives: vec![] }
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

        println!("{:#?}", self.primitives);

        Ok(())
    }
}
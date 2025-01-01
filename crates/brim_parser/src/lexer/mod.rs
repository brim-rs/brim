use brim::cursor::Cursor;
use brim::files::SimpleFile;
use brim::index::{ByteIndex, ByteOffset, RawOffset};
use brim::PrimitiveToken;
use brim::session::Session;

#[derive(Debug)]
pub struct Lexer<'a> {
    pos: ByteIndex,
    session: &'a Session<'a>,
    file: &'a SimpleFile,
    primitives: &'a mut Vec<PrimitiveToken>,
}

impl Lexer<'_> {
    pub fn new<'a>(session: &'a Session<'a>, file: &'a SimpleFile, primitives: &'a mut Vec<PrimitiveToken>) -> Lexer<'a> {
        Lexer {
            pos: ByteIndex::default(),
            session,
            file,
            primitives,
        }
    }
}

impl Lexer<'_> {
    pub fn next_token(&mut self) -> Option<PrimitiveToken> {
        let token = self.primitives.remove(0);
        let start = self.pos;

        self.pos = start + ByteOffset(token.len as RawOffset);

        Some(token)
    }
}
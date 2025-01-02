mod comments;

use brim::cursor::Cursor;
use brim::files::SimpleFile;
use brim::index::{ByteIndex, ByteOffset, RawOffset};
use brim::{PrimitiveToken, PrimitiveTokenKind};
use brim::session::Session;
use brim::span::Span;
use brim::symbol::Symbol;
use brim::token::{Token, TokenKind};

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
    pub fn next_token(&mut self) -> Option<Token> {
        if self.primitives.is_empty() {
            return None;
        }

        let token = self.primitives.remove(0);
        let start = self.pos;

        self.pos = start + ByteOffset(token.len as RawOffset);

        let kind = match token.kind {
            PrimitiveTokenKind::Whitespace | PrimitiveTokenKind::Comment {
                doc: false,
            } => TokenKind::Skipable,
            PrimitiveTokenKind::Comment { doc: true } => {
                let comment_start = start + ByteOffset(3);
                let content = self.content_from(comment_start);

                TokenKind::DocComment(Symbol::new(content))
            }
            _ => TokenKind::Colon
        };

        let span = Span::new(start, self.pos);
        Some(Token::new(kind, span))
    }

    pub fn content_from_to(
        &self, start: ByteIndex, end: ByteIndex,
    ) -> &str {
        let start = start.to_usize();
        let end = end.to_usize();
        &self.file.source()[start..end]
    }

    pub fn content_from(&self, start: ByteIndex) -> &str {
        self.content_from_to(start, self.pos)
    }
}
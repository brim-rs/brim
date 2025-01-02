use brim::cursor::Cursor;
use brim::files::SimpleFile;
use brim::index::{ByteIndex, ByteOffset, RawOffset};
use brim::{PrimitiveToken, PrimitiveTokenKind};
use brim::session::Session;
use brim::span::Span;
use brim::symbol::Symbol;
use brim::token::{BinOpToken, Delimiter, Orientation, Token, TokenKind};
use brim::token::TokenKind::BinOp;

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

            // Delimiters
            PrimitiveTokenKind::OpenParen => TokenKind::Delimiter(Delimiter::Paren, Orientation::Open),
            PrimitiveTokenKind::CloseParen => TokenKind::Delimiter(Delimiter::Paren, Orientation::Close),
            PrimitiveTokenKind::OpenBrace => TokenKind::Delimiter(Delimiter::Brace, Orientation::Open),
            PrimitiveTokenKind::CloseBrace => TokenKind::Delimiter(Delimiter::Brace, Orientation::Close),
            PrimitiveTokenKind::OpenBracket => TokenKind::Delimiter(Delimiter::Bracket, Orientation::Open),
            PrimitiveTokenKind::CloseBracket => TokenKind::Delimiter(Delimiter::Bracket, Orientation::Close),

            // Binary ops
            PrimitiveTokenKind::Minus => TokenKind::BinOp(BinOpToken::Minus),
            PrimitiveTokenKind::Ampersand => TokenKind::BinOp(BinOpToken::And),
            PrimitiveTokenKind::Pipe => TokenKind::BinOp(BinOpToken::Or),
            PrimitiveTokenKind::Plus => TokenKind::BinOp(BinOpToken::Plus),
            PrimitiveTokenKind::Asterisk => TokenKind::BinOp(BinOpToken::Star),
            PrimitiveTokenKind::Slash => TokenKind::BinOp(BinOpToken::Slash),
            PrimitiveTokenKind::Caret => TokenKind::BinOp(BinOpToken::Caret),
            PrimitiveTokenKind::Percent => TokenKind::BinOp(BinOpToken::Percent),

            // Symbols
            PrimitiveTokenKind::Semicolon => TokenKind::Semicolon,
            PrimitiveTokenKind::Comma => TokenKind::Comma,
            PrimitiveTokenKind::Dot => TokenKind::Dot,
            PrimitiveTokenKind::At => TokenKind::At,
            PrimitiveTokenKind::Tilde => TokenKind::Tilde,
            PrimitiveTokenKind::QuestionMark => TokenKind::QuestionMark,
            PrimitiveTokenKind::Colon => TokenKind::Colon,
            PrimitiveTokenKind::Dollar => TokenKind::Dollar,

            // Comparison
            PrimitiveTokenKind::Equals => TokenKind::Eq,
            PrimitiveTokenKind::Bang => TokenKind::Bang,
            PrimitiveTokenKind::LessThan => TokenKind::Lt,
            PrimitiveTokenKind::GreaterThan => TokenKind::Gt,

            _ => TokenKind::Skipable
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
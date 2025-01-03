mod errors;
mod identifiers;
mod unicode;
mod literals;

use crate::lexer::{errors::EmojiIdentifier, identifiers::nfc_normalize, unicode::UNICODE_ARRAY};
use brim::{PrimitiveToken, PrimitiveTokenKind, files::SimpleFile, index::{ByteIndex, ByteOffset, RawOffset}, session::Session, span::Span, symbol::Symbol, token::{BinOpToken, Delimiter, Orientation, Token, TokenKind}, LiteralKind};
use brim::compiler::CompilerContext;
use brim::token::Lit;

#[derive(Debug)]
pub struct Lexer<'a> {
    pos: ByteIndex,
    file: &'a SimpleFile,
    primitives: &'a mut Vec<PrimitiveToken>,
}

impl Lexer<'_> {
    pub fn new<'a>(file: &'a SimpleFile, primitives: &'a mut Vec<PrimitiveToken>) -> Lexer<'a> {
        Lexer {
            pos: ByteIndex::default(),
            file,
            primitives,
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn next_token(&mut self, comp: &mut CompilerContext) -> Option<Token> {
        if self.primitives.is_empty() {
            return None;
        }

        let token = self.primitives.remove(0);
        let start = self.pos;

        self.pos = start + ByteOffset(token.len as RawOffset);

        let kind = match token.kind {
            PrimitiveTokenKind::Whitespace | PrimitiveTokenKind::Comment { doc: false } => {
                TokenKind::Skipable
            }
            PrimitiveTokenKind::Comment { doc: true } => {
                let comment_start = start + ByteOffset(3);
                let content = self.content_from(comment_start);

                TokenKind::DocComment(Symbol::new(content))
            }

            PrimitiveTokenKind::Ident => self.ident(start),

            PrimitiveTokenKind::InvalidIdent
            if !UNICODE_ARRAY.iter().any(|&(c, _, _)| {
                let sym = self.content_from(start);
                sym.chars().count() == 1 && c == sym.chars().next().unwrap()
            }) =>
                {
                    let symbol = nfc_normalize(self.content_from(start));
                    let span = Span::new(start, self.pos);

                    session.emit(EmojiIdentifier { ident: symbol, label: (span, self.file.id()) });

                    TokenKind::Ident(symbol)
                }

            PrimitiveTokenKind::Literal {
                kind,
                suffix_start,
            } => {
                let suffix_start = start + ByteOffset(suffix_start as RawOffset);
                let (kind, symbol) = self.lex_literal(kind, start, suffix_start, session);
                let suffix = if suffix_start < self.pos {
                    let string = self.content_from(suffix_start);
                    if string == "_" {
                        // self.dcx().emit_err(errors::UnderscoreLiteralSuffix {
                        //     span: Span::new(suffix_start, self.pos),
                        // });
                        None
                    } else {
                        Some(Symbol::new(string))
                    }
                } else {
                    None
                };
                TokenKind::Literal(Lit { kind, symbol, suffix })
            }

            // Delimiters
            PrimitiveTokenKind::OpenParen => {
                TokenKind::Delimiter(Delimiter::Paren, Orientation::Open)
            }
            PrimitiveTokenKind::CloseParen => {
                TokenKind::Delimiter(Delimiter::Paren, Orientation::Close)
            }
            PrimitiveTokenKind::OpenBrace => {
                TokenKind::Delimiter(Delimiter::Brace, Orientation::Open)
            }
            PrimitiveTokenKind::CloseBrace => {
                TokenKind::Delimiter(Delimiter::Brace, Orientation::Close)
            }
            PrimitiveTokenKind::OpenBracket => {
                TokenKind::Delimiter(Delimiter::Bracket, Orientation::Open)
            }
            PrimitiveTokenKind::CloseBracket => {
                TokenKind::Delimiter(Delimiter::Bracket, Orientation::Close)
            }

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

            _ => TokenKind::Skipable,
        };

        let span = Span::new(start, self.pos);
        Some(Token::new(kind, span))
    }

    pub fn content_from_to(&self, start: ByteIndex, end: ByteIndex) -> &str {
        let start = start.to_usize();
        let end = end.to_usize();
        &self.file.source()[start..end]
    }

    pub fn content_from(&self, start: ByteIndex) -> &str {
        self.content_from_to(start, self.pos)
    }
}

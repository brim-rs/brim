mod errors;
mod identifiers;
mod literals;
mod unicode;

use crate::lexer::{errors::EmojiIdentifier, identifiers::nfc_normalize, unicode::UNICODE_ARRAY};
use brim::{
    PrimitiveToken, PrimitiveTokenKind,
    compiler::CompilerContext,
    files::SimpleFile,
    index::{ByteIndex, ByteOffset, RawOffset},
    span::Span,
    symbols::Symbol,
    token::{BinOpToken, Delimiter, Lit, Orientation, Token, TokenKind},
};

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

                    comp.emit(EmojiIdentifier {
                        ident: symbol,
                        label: (span, self.file.id()),
                    });

                    TokenKind::Ident(symbol)
                }

            PrimitiveTokenKind::Literal { kind, suffix_start } => {
                let suffix_start = start + ByteOffset(suffix_start as RawOffset);
                let (kind, symbol) = self.lex_literal(kind, start, suffix_start, comp);
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
                TokenKind::Literal(Lit {
                    kind,
                    symbol,
                    suffix,
                })
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

            // Compound token handlers
            PrimitiveTokenKind::Minus => self.try_lex_arrow(),
            PrimitiveTokenKind::Equals => self.try_lex_double_equals(),
            PrimitiveTokenKind::Bang => self.try_lex_not_equals(),
            PrimitiveTokenKind::GreaterThan => self.try_lex_greater_equals(),
            PrimitiveTokenKind::LessThan => self.try_lex_less_equals(),

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
            
            PrimitiveTokenKind::Unknown => {
                let span = Span::new(start, self.pos);
                let content = self.content_from(start);
                comp.emit(errors::UnknownToken {
                    span: (span, self.file.id()),
                    token: content.to_string(),
                });

                TokenKind::Skipable
            }

            PrimitiveTokenKind::Eof => TokenKind::Eof,
            _ => TokenKind::Skipable,
        };

        let span = Span::new(start, self.pos);
        Some(Token::new(kind, span))
    }

    // Arrow token ->
    fn try_lex_arrow(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::GreaterThan,
            TokenKind::Arrow,
            TokenKind::BinOp(BinOpToken::Minus),
        )
    }

    // Double equals ==
    fn try_lex_double_equals(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::EqEq,
            TokenKind::Eq,
        )
    }

    // Not equals !=
    fn try_lex_not_equals(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::Ne,
            TokenKind::Bang,
        )
    }

    // Greater than or equals >=
    fn try_lex_greater_equals(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::Ge,
            TokenKind::Gt,
        )
    }

    // Less than or equals <=
    fn try_lex_less_equals(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::Le,
            TokenKind::Lt,
        )
    }

    pub fn content_from_to(&self, start: ByteIndex, end: ByteIndex) -> &str {
        let start = start.to_usize();
        let end = end.to_usize();
        &self.file.source()[start..end]
    }

    pub fn try_compound_token(&mut self, next_kind: PrimitiveTokenKind, compound_token: TokenKind, default_token: TokenKind) -> TokenKind {
        if !self.primitives.is_empty() && self.primitives[0].kind == next_kind {
            // Consume the second token
            self.primitives.remove(0);
            self.pos = self.pos + ByteOffset(1);
            compound_token
        } else {
            default_token
        }
    }

    pub fn content_from(&self, start: ByteIndex) -> &str {
        self.content_from_to(start, self.pos)
    }
}

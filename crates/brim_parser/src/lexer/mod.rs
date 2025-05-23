mod errors;
mod identifiers;
mod literals;
mod unicode;

use crate::lexer::{errors::EmojiIdentifier, identifiers::nfc_normalize, unicode::UNICODE_ARRAY};
use brim_ast::{
    item::Ident,
    token::{AssignOpToken, BinOpToken, Delimiter, Lit, Orientation, Token, TokenKind},
};
use brim_lexer::{PrimitiveToken, PrimitiveTokenKind};
use brim_middle::temp_diag::TemporaryDiagnosticContext;
use brim_span::{
    files::SimpleFile,
    index::{ByteIndex, ByteOffset, RawOffset},
    span::Span,
    symbols::Symbol,
};

#[derive(Debug)]
pub struct Lexer<'a> {
    pos: ByteIndex,
    file: &'a SimpleFile,
    primitives: Vec<PrimitiveToken>,
    pub ctx: TemporaryDiagnosticContext,
}

impl<'a> Lexer<'a> {
    pub fn new(
        file: &'a SimpleFile,
        primitives: Vec<PrimitiveToken>,
        lex_temp: TemporaryDiagnosticContext,
    ) -> Lexer<'a> {
        Lexer { pos: ByteIndex::default(), file, primitives, ctx: lex_temp }
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
            PrimitiveTokenKind::Whitespace => TokenKind::Skipable,
            PrimitiveTokenKind::Comment { doc } => {
                if doc {
                    let comment_start = start + ByteOffset(3);
                    let content = self.content_from(comment_start);
                    TokenKind::DocComment(Symbol::new(content))
                } else {
                    TokenKind::Comment
                }
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
                self.ctx
                    .emit_impl(EmojiIdentifier { ident: symbol, label: (span, self.file.id()) });
                TokenKind::Ident(symbol)
            }

            PrimitiveTokenKind::Literal { kind, suffix_start } => {
                let suffix_start = start + ByteOffset(suffix_start as RawOffset);
                let (kind, symbol) = self.lex_literal(kind, start, suffix_start);
                let suffix = if suffix_start < self.pos {
                    let string = self.content_from(suffix_start);
                    if string == "_" {
                        None
                    } else {
                        Some(Ident::new(Symbol::new(string), Span::new(suffix_start, self.pos)))
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

            // Compound token handlers with assignment operators
            PrimitiveTokenKind::Plus => self.try_lex_plus_assign(),
            PrimitiveTokenKind::Minus => self.try_lex_minus_assign(),
            PrimitiveTokenKind::Asterisk => self.try_lex_star_assign(),
            PrimitiveTokenKind::Slash => self.try_lex_slash_assign(),
            PrimitiveTokenKind::Percent => self.try_lex_mod_assign(),
            PrimitiveTokenKind::Caret => self.try_lex_caret_assign(),
            PrimitiveTokenKind::Ampersand => self.try_lex_and_assign(),
            PrimitiveTokenKind::Pipe => self.try_lex_or_assign(),
            PrimitiveTokenKind::LessThan => self.try_lex_shift_left_assign(),
            PrimitiveTokenKind::GreaterThan => self.try_lex_shift_right_assign(),
            PrimitiveTokenKind::Equals => self.try_lex_starting_with_equals(),
            PrimitiveTokenKind::Bang => self.try_lex_not_equals(),

            // Symbols
            PrimitiveTokenKind::Semicolon => TokenKind::Semicolon,
            PrimitiveTokenKind::Comma => TokenKind::Comma,
            PrimitiveTokenKind::Dot => TokenKind::Dot,
            PrimitiveTokenKind::At => TokenKind::At,
            PrimitiveTokenKind::Tilde => TokenKind::Tilde,
            PrimitiveTokenKind::QuestionMark => TokenKind::QuestionMark,
            PrimitiveTokenKind::Colon => self.try_lex_colon_or_double_colon(),
            PrimitiveTokenKind::Dollar => TokenKind::Dollar,

            PrimitiveTokenKind::Unknown => {
                let span = Span::new(start, self.pos);
                let content = self.content_from(start);
                self.ctx.emit_impl(errors::UnknownToken {
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

    fn try_lex_colon_or_double_colon(&mut self) -> TokenKind {
        self.try_multi_char_token(
            &[(PrimitiveTokenKind::Colon, TokenKind::DoubleColon)],
            TokenKind::Colon,
        )
    }

    fn try_lex_plus_assign(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::AssignOp(AssignOpToken::PlusEq),
            TokenKind::BinOp(BinOpToken::Plus),
        )
    }

    fn try_lex_minus_assign(&mut self) -> TokenKind {
        self.try_multi_char_token(
            &[
                (PrimitiveTokenKind::GreaterThan, TokenKind::Arrow),
                (PrimitiveTokenKind::Equals, TokenKind::AssignOp(AssignOpToken::MinusEq)),
            ],
            TokenKind::BinOp(BinOpToken::Minus),
        )
    }

    fn try_lex_shift_left_assign(&mut self) -> TokenKind {
        self.try_three_char_token(
            PrimitiveTokenKind::LessThan,
            PrimitiveTokenKind::Equals,
            TokenKind::AssignOp(AssignOpToken::ShlEq),
            TokenKind::BinOp(BinOpToken::ShiftLeft),
            (PrimitiveTokenKind::Equals, TokenKind::Le),
            TokenKind::Lt,
        )
    }

    fn try_lex_shift_right_assign(&mut self) -> TokenKind {
        self.try_three_char_token(
            PrimitiveTokenKind::GreaterThan,
            PrimitiveTokenKind::Equals,
            TokenKind::AssignOp(AssignOpToken::ShrEq),
            TokenKind::BinOp(BinOpToken::ShiftRight),
            (PrimitiveTokenKind::Equals, TokenKind::Ge),
            TokenKind::Gt,
        )
    }

    /// Can be either: `**`, `*`, or `*=`
    fn try_lex_star_assign(&mut self) -> TokenKind {
        // First check if we have a double asterisk '**'
        if !self.primitives.is_empty() && self.primitives[0].kind == PrimitiveTokenKind::Asterisk {
            self.primitives.remove(0);
            self.pos += ByteOffset(1);
            return TokenKind::BinOp(BinOpToken::Power);
        }

        // If not double asterisk, check if it's '*='
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::AssignOp(AssignOpToken::StarEq),
            TokenKind::BinOp(BinOpToken::Star),
        )
    }

    fn try_lex_slash_assign(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::AssignOp(AssignOpToken::SlashEq),
            TokenKind::BinOp(BinOpToken::Slash),
        )
    }

    fn try_lex_mod_assign(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::AssignOp(AssignOpToken::ModEq),
            TokenKind::BinOp(BinOpToken::Percent),
        )
    }

    fn try_lex_caret_assign(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::AssignOp(AssignOpToken::CaretEq),
            TokenKind::BinOp(BinOpToken::Caret),
        )
    }

    fn try_lex_and_assign(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::AssignOp(AssignOpToken::AndEq),
            TokenKind::BinOp(BinOpToken::And),
        )
    }

    fn try_lex_or_assign(&mut self) -> TokenKind {
        self.try_compound_token(
            PrimitiveTokenKind::Equals,
            TokenKind::AssignOp(AssignOpToken::OrEq),
            TokenKind::BinOp(BinOpToken::Or),
        )
    }

    /// One of: `=>`, `=`, or `==`
    fn try_lex_starting_with_equals(&mut self) -> TokenKind {
        self.try_multi_char_token(
            &[
                (PrimitiveTokenKind::GreaterThan, TokenKind::FatArrow),
                (PrimitiveTokenKind::Equals, TokenKind::EqEq),
            ],
            TokenKind::Eq,
        )
    }

    fn try_lex_not_equals(&mut self) -> TokenKind {
        self.try_compound_token(PrimitiveTokenKind::Equals, TokenKind::Ne, TokenKind::Bang)
    }

    fn try_multi_char_token(
        &mut self,
        options: &[(PrimitiveTokenKind, TokenKind)],
        default: TokenKind,
    ) -> TokenKind {
        if !self.primitives.is_empty() {
            let next_kind = self.primitives[0].kind;
            for &(kind, ref token) in options {
                if next_kind == kind {
                    self.primitives.remove(0);
                    self.pos += ByteOffset(1);
                    return token.clone();
                }
            }
        }
        default
    }

    fn try_three_char_token(
        &mut self,
        second_char: PrimitiveTokenKind,
        third_char: PrimitiveTokenKind,
        three_char_result: TokenKind,
        two_char_result: TokenKind,
        single_char_option: (PrimitiveTokenKind, TokenKind),
        default: TokenKind,
    ) -> TokenKind {
        if self.primitives.is_empty() {
            return default;
        }

        if self.primitives[0].kind == second_char {
            self.primitives.remove(0);
            self.pos += ByteOffset(1);

            if !self.primitives.is_empty() && self.primitives[0].kind == third_char {
                self.primitives.remove(0);
                self.pos += ByteOffset(1);
                three_char_result
            } else {
                two_char_result
            }
        } else if self.primitives[0].kind == single_char_option.0 {
            self.primitives.remove(0);
            self.pos += ByteOffset(1);
            single_char_option.1
        } else {
            default
        }
    }

    pub fn content_from_to(&self, start: ByteIndex, end: ByteIndex) -> &str {
        let start = start.to_usize();
        let end = end.to_usize();
        &self.file.source()[start..end]
    }

    pub fn try_compound_token(
        &mut self,
        next_kind: PrimitiveTokenKind,
        compound_token: TokenKind,
        default_token: TokenKind,
    ) -> TokenKind {
        if !self.primitives.is_empty() && self.primitives[0].kind == next_kind {
            self.primitives.remove(0);
            self.pos += ByteOffset(1);
            compound_token
        } else {
            default_token
        }
    }

    pub fn content_from(&self, start: ByteIndex) -> &str {
        self.content_from_to(start, self.pos)
    }
}

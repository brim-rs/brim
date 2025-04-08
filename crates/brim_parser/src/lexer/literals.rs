use crate::lexer::{
    Lexer,
    errors::{
        EmptyExponent, InvalidDigitLiteral, NoDigitsLiteral, UnsupportedFloatBase,
        UnterminatedLiteral,
    },
};
use brim_ast::token::LitKind;
use brim_lexer::{Base, LiteralKind};
use brim_span::{
    index::{ByteIndex, ByteOffset},
    span::Span,
    symbols::Symbol,
};

impl Lexer<'_> {
    pub fn lex_literal(
        &mut self,
        lit: LiteralKind,
        start: ByteIndex,
        end: ByteIndex,
    ) -> (LitKind, Symbol) {
        let content = self.content_from_to(start, end).to_string();

        match lit {
            LiteralKind::Int { base, empty_int } => {
                let symbol = Symbol::from(content.clone());

                let kind = if empty_int {
                    self.handle_empty_int(start, end)
                } else if matches!(base, Base::Binary | Base::Octal) {
                    // Get the slice without the prefix (0b or 0o)
                    let digits = &content[2..];

                    self.validate_digits(start, base as u32, digits)
                } else {
                    LitKind::Integer
                };

                (kind, symbol)
            }
            LiteralKind::Float { base, empty_exponent } => {
                self.handle_float(base, empty_exponent, start, end)
            }
            // TODO: In the future, validate the content of the string. Unescape etc.
            LiteralKind::Byte { terminated } => self.handle_byte(terminated, start, end),
            LiteralKind::ByteStr { terminated } => self.handle_byte_str(terminated, start, end),
            LiteralKind::Str { terminated } => self.handle_str(terminated, start, end),
            LiteralKind::Char { terminated } => self.handle_char(terminated, start, end),
        }
    }

    pub fn handle_char(
        &mut self,
        terminated: bool,
        start: ByteIndex,
        end: ByteIndex,
    ) -> (LitKind, Symbol) {
        let kind = if !terminated {
            let span = Span::new(start, end);
            let emitted = self
                .ctx
                .emit_impl(UnterminatedLiteral { span: (span, self.file.id()), type_: "char" });
            LitKind::Err(emitted)
        } else {
            LitKind::Char
        };

        let content = self
            .content_from_to(start + ByteOffset::from_usize(1), end - ByteOffset::from_usize(1));
        (kind, Symbol::new(content))
    }

    pub fn handle_str(
        &mut self,
        terminated: bool,
        start: ByteIndex,
        end: ByteIndex,
    ) -> (LitKind, Symbol) {
        let kind = if !terminated {
            let span = Span::new(start, end);
            let emitted = self
                .ctx
                .emit_impl(UnterminatedLiteral { span: (span, self.file.id()), type_: "string" });
            LitKind::Err(emitted)
        } else {
            LitKind::Str
        };

        let content = self
            .content_from_to(start + ByteOffset::from_usize(1), end - ByteOffset::from_usize(1));
        (kind, Symbol::new(content))
    }

    fn handle_float(
        &mut self,
        base: Base,
        empty_exponent: bool,
        start: ByteIndex,
        end: ByteIndex,
    ) -> (LitKind, Symbol) {
        let mut kind = LitKind::Float;

        if empty_exponent {
            let span = Span::new(start, end);
            let emitted = self.ctx.emit_impl(EmptyExponent { span: (span, self.file.id()) });
            kind = LitKind::Err(emitted);
        }

        if base != Base::Decimal {
            let span = Span::new(start, end);
            let emitted =
                self.ctx.emit_impl(UnsupportedFloatBase { span: (span, self.file.id()), base });
            kind = LitKind::Err(emitted)
        }
        (kind, Symbol::new(self.content_from_to(start, end)))
    }

    fn handle_byte_str(
        &mut self,
        terminated: bool,
        start: ByteIndex,
        end: ByteIndex,
    ) -> (LitKind, Symbol) {
        let kind = if !terminated {
            let span = Span::new(start, end);
            let emitted = self.ctx.emit_impl(UnterminatedLiteral {
                span: (span, self.file.id()),
                type_: "byte string",
            });
            LitKind::Err(emitted)
        } else {
            LitKind::ByteStr
        };

        let content = self
            .content_from_to(start + ByteOffset::from_usize(2), end - ByteOffset::from_usize(1));
        (kind, Symbol::new(content))
    }

    fn handle_byte(
        &mut self,
        terminated: bool,
        start: ByteIndex,
        end: ByteIndex,
    ) -> (LitKind, Symbol) {
        let kind = if !terminated {
            let span = Span::new(start, end);
            let emitted = self
                .ctx
                .emit_impl(UnterminatedLiteral { span: (span, self.file.id()), type_: "byte" });
            LitKind::Err(emitted)
        } else {
            LitKind::Byte
        };

        (
            kind,
            Symbol::new(self.content_from_to(
                start + ByteOffset::from_usize(2),
                end - ByteOffset::from_usize(1),
            )),
        )
    }

    fn handle_empty_int(&mut self, start: ByteIndex, end: ByteIndex) -> LitKind {
        let span = Span::new(start, end);
        let emitted = self.ctx.emit_impl(NoDigitsLiteral { span: (span, self.file.id()) });
        LitKind::Err(emitted)
    }

    fn validate_digits(&mut self, start: ByteIndex, base: u32, digits: &str) -> LitKind {
        let mut kind = LitKind::Integer;

        for (idx, c) in digits.char_indices() {
            if c == '_' {
                continue;
            }

            if c.to_digit(base).is_none() {
                let span = Span::new(
                    start + ByteOffset::from_usize(2 + idx),
                    start + ByteOffset::from_usize(2 + idx + c.len_utf8()),
                );
                kind = LitKind::Err(
                    self.ctx.emit_impl(InvalidDigitLiteral { span: (span, self.file.id()), base }),
                );
            }
        }

        kind
    }
}

use crate::lexer::{
    Lexer,
    errors::{
        EmptyExponent, InvalidDigitLiteral, NoDigitsLiteral, UnsupportedFloatBase,
        UnterminatedLiteral,
    },
};
use brim_ast::{item::Ident, token::LitKind};
use brim_ctx::compiler::CompilerContext;
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
        comp: &mut CompilerContext,
    ) -> (LitKind, Ident) {
        let span = Span::new(start, end);
        match lit {
            LiteralKind::Int { base, empty_int } => {
                let content = self.content_from_to(start, end);
                let symbol = Symbol::from(content);
                let kind = if empty_int {
                    self.handle_empty_int(start, end, comp)
                } else if matches!(base, Base::Binary | Base::Octal) {
                    let digits = &content[2..];
                    self.validate_digits(start, base as u32, digits, comp)
                } else {
                    LitKind::Integer
                };
                (kind, Ident::new(symbol, span))
            }
            LiteralKind::Float {
                base,
                empty_exponent,
            } => self.handle_float(base, empty_exponent, start, end, comp),
            LiteralKind::Byte { terminated } => self.handle_byte(terminated, start, end, comp),
            LiteralKind::ByteStr { terminated } => {
                self.handle_byte_str(terminated, start, end, comp)
            }
            LiteralKind::Str { terminated } => self.handle_str(terminated, start, end, comp),
            LiteralKind::Char { terminated } => self.handle_char(terminated, start, end, comp),
        }
    }

    pub fn handle_char(
        &self,
        terminated: bool,
        start: ByteIndex,
        end: ByteIndex,
        comp: &mut CompilerContext,
    ) -> (LitKind, Ident) {
        let span = Span::new(start, end);
        let kind = if !terminated {
            let emitted = comp.emit(UnterminatedLiteral {
                span: (span, self.file.id()),
                type_: "char",
            });
            LitKind::Err(emitted)
        } else {
            LitKind::Char
        };
        let content = self.content_from_to(
            start + ByteOffset::from_usize(1),
            end - ByteOffset::from_usize(1),
        );
        (kind, Ident::new(Symbol::new(content), span))
    }

    fn handle_float(
        &self,
        base: Base,
        empty_exponent: bool,
        start: ByteIndex,
        end: ByteIndex,
        comp: &mut CompilerContext,
    ) -> (LitKind, Ident) {
        let span = Span::new(start, end);
        let mut kind = LitKind::Float;
        if empty_exponent {
            kind = LitKind::Err(comp.emit(EmptyExponent {
                span: (span, self.file.id()),
            }));
        }
        if base != Base::Decimal {
            kind = LitKind::Err(comp.emit(UnsupportedFloatBase {
                span: (span, self.file.id()),
                base,
            }));
        }
        (
            kind,
            Ident::new(Symbol::new(self.content_from_to(start, end)), span),
        )
    }

    fn handle_byte(
        &self,
        terminated: bool,
        start: ByteIndex,
        end: ByteIndex,
        comp: &mut CompilerContext,
    ) -> (LitKind, Ident) {
        let span = Span::new(start, end);
        let kind = if !terminated {
            LitKind::Err(comp.emit(UnterminatedLiteral {
                span: (span, self.file.id()),
                type_: "byte",
            }))
        } else {
            LitKind::Byte
        };
        (
            kind,
            Ident::new(
                Symbol::new(self.content_from_to(
                    start + ByteOffset::from_usize(2),
                    end - ByteOffset::from_usize(1),
                )),
                span,
            ),
        )
    }

    fn handle_byte_str(
        &self,
        terminated: bool,
        start: ByteIndex,
        end: ByteIndex,
        comp: &mut CompilerContext,
    ) -> (LitKind, Ident) {
        let span = Span::new(start, end);
        let kind = if !terminated {
            LitKind::Err(comp.emit(UnterminatedLiteral {
                span: (span, self.file.id()),
                type_: "byte string",
            }))
        } else {
            LitKind::ByteStr
        };
        (
            kind,
            Ident::new(
                Symbol::new(self.content_from_to(
                    start + ByteOffset::from_usize(2),
                    end - ByteOffset::from_usize(1),
                )),
                span,
            ),
        )
    }

    fn handle_str(
        &self,
        terminated: bool,
        start: ByteIndex,
        end: ByteIndex,
        comp: &mut CompilerContext,
    ) -> (LitKind, Ident) {
        let span = Span::new(start, end);
        let kind = if !terminated {
            LitKind::Err(comp.emit(UnterminatedLiteral {
                span: (span, self.file.id()),
                type_: "string",
            }))
        } else {
            LitKind::Str
        };
        (
            kind,
            Ident::new(
                Symbol::new(self.content_from_to(
                    start + ByteOffset::from_usize(1),
                    end - ByteOffset::from_usize(1),
                )),
                span,
            ),
        )
    }

    fn handle_empty_int(
        &self,
        start: ByteIndex,
        end: ByteIndex,
        comp: &mut CompilerContext,
    ) -> LitKind {
        let span = Span::new(start, end);
        LitKind::Err(comp.emit(NoDigitsLiteral {
            span: (span, self.file.id()),
        }))
    }

    fn validate_digits(
        &self,
        start: ByteIndex,
        base: u32,
        digits: &str,
        comp: &mut CompilerContext,
    ) -> LitKind {
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
                kind = LitKind::Err(comp.emit(InvalidDigitLiteral {
                    span: (span, self.file.id()),
                    base,
                }));
            }
        }
        kind
    }
}

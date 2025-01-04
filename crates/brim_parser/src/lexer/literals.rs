use crate::lexer::{
    Lexer,
    errors::{InvalidDigitLiteral, NoDigitsLiteral},
};
use brim::{
    Base, LiteralKind,
    compiler::CompilerContext,
    index::{ByteIndex, ByteOffset},
    span::Span,
    symbol::Symbol,
    token::LitKind,
};

impl Lexer<'_> {
    pub fn lex_literal(
        &mut self,
        lit: LiteralKind,
        start: ByteIndex,
        end: ByteIndex,
        comp: &mut CompilerContext,
    ) -> (LitKind, Symbol) {
        match lit {
            LiteralKind::Int { base, empty_int } => {
                let content = self.content_from_to(start, end);
                let symbol = Symbol::from(content);

                let kind = if empty_int {
                    self.handle_empty_int(start, end, comp)
                } else if matches!(base, Base::Binary | Base::Octal) {
                    // Get the slice without the prefix (0b or 0o)
                    let digits = &content[2..];
                    self.validate_digits(start, base as u32, digits, comp)
                } else {
                    LitKind::Integer
                };

                (kind, symbol)
            }
            _ => todo!("other literals"),
        }
    }

    fn handle_empty_int(
        &self,
        start: ByteIndex,
        end: ByteIndex,
        comp: &mut CompilerContext,
    ) -> LitKind {
        let span = Span::new(start, end);
        let emitted = comp.emit(NoDigitsLiteral {
            span: (span, self.file.id()),
        });
        LitKind::Err(emitted)
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

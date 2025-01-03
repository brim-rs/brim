use brim::{
    Diagnostic,
    diagnostic::{Severity, ToDiagnostic, Label, LabelStyle},
    symbol::Symbol,
};
use brim::span::Span;

#[derive(Diagnostic)]
#[error("Emojis can't be used in identifiers: `{ident}`")]
pub struct EmojiIdentifier {
    pub ident: Symbol,
    #[error("invalid identifier")]
    pub label: (Span, usize),
}

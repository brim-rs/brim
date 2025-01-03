use brim::{
    Diagnostic,
    diagnostic::{Severity, ToDiagnostic},
    symbol::Symbol,
};

#[derive(Diagnostic)]
#[error("Emojis can't be used in identifiers: `{ident}`")]
pub struct EmojiIdentifier {
    pub ident: Symbol,
}

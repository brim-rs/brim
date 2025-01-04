use brim::{Diagnostic, diagnostic::{Label, LabelStyle, Severity, ToDiagnostic}, span::Span, symbol::Symbol, Base};

#[derive(Diagnostic)]
#[error("Emojis can't be used in identifiers: `{ident}`")]
pub struct EmojiIdentifier {
    pub ident: Symbol,
    #[error("invalid identifier")]
    pub label: (Span, usize),
}

#[derive(Diagnostic)]
#[error("Literal has no digits")]
pub struct NoDigitsLiteral {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("Invalid digit literal for base `{base}`")]
pub struct InvalidDigitLiteral {
    #[error]
    pub span: (Span, usize),
    pub base: u32,
}

#[derive(Diagnostic)]
#[error("Found empty float exponent. Expected at least one digit.")]
pub struct EmptyExponent {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("Unsupported float base `{base}`")]
pub struct UnsupportedFloatBase {
    #[error]
    pub span: (Span, usize),
    pub base: Base
}
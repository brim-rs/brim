use brim::Diagnostic;
use brim::span::Span;
use brim::diagnostic::{Severity, LabelStyle, Label, ToDiagnostic};

#[derive(Diagnostic)]
#[error("Invalid function signature. {message}")]
pub struct InvalidFunctionSignature {
    pub(crate) message: String,
    #[error("invalid signature")]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("Invalid modifier order. {message}")]
pub struct InvalidModifierOrder {
    pub(crate) message: String,
    #[error("invalid modifier")]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("expected identifier. {message}")]
pub struct ExpectedIdentifier {
    pub(crate) message: String,
    #[error]
    pub span: (Span, usize),
}
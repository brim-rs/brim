use brim::Diagnostic;
use brim::span::Span;
use brim::diagnostic::{Severity, LabelStyle, Label, ToDiagnostic};
use brim::token::TokenKind;

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

// TODO: use `add` label to show `>` in the diagnostic
#[derive(Diagnostic)]
#[error("expected closing `>` for generics.")]
pub struct ExpectedClosingGenerics {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("expected `{expected}`, but found `{found}`.")]
pub struct ExpectedToken {
    pub expected: TokenKind,
    pub found: TokenKind,
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("`const` should be placed before reference of pointer.")]
pub struct ConstAfter {
    #[error("move `const` before {before}")]
    pub span: (Span, usize),
    pub before: &'static str,
}
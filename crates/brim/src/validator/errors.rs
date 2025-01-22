use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;

#[derive(Diagnostic)]
#[error("function declaration exceeds 255 parameters")]
pub struct TooManyParameters {
    #[error("exceeded 255 parameters")]
    pub span: (Span, usize),
    #[note]
    pub note: &'static str,
}

#[derive(Diagnostic)]
#[error("duplicate parameter `{name}`")]
pub struct DuplicateParam {
    #[error("first defined here")]
    pub span: (Span, usize),
    #[error("duplicate found here")]
    pub dup: (Span, usize),
    pub name: String
}
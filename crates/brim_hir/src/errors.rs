use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;

#[derive(Diagnostic)]
#[error("comptime expected to return a type, found literal instead")]
pub struct ComptimeExpectedType {
    #[error]
    pub span: (Span, usize),
}

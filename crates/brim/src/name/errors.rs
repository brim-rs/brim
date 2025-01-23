use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;

#[derive(Diagnostic)]
#[error("found redeclaration of variable `{name}`")]
pub struct RedeclaredVariable {
    #[error("first defined here")]
    pub span: (Span, usize),
    #[error("redeclared here")]
    pub redecl: (Span, usize),
    pub name: String
}

#[derive(Diagnostic)]
#[error("undeclared variable `{name}`")]
pub struct UndeclaredVariable {
    #[error("variable `{name}` is not declared in this scope")]
    pub span: (Span, usize),
    pub name: String
}
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;

#[derive(Diagnostic)]
#[error("entrypoint file: '{file}' doesn't contain a main function")]
pub struct NoMainFunction {
    pub file: String,
}

#[derive(Diagnostic)]
#[error("main function cannot have parameters")]
pub struct MainFunctionParams {
    #[error("delete the parameters")]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("main function cannot be constant")]
pub struct MainFunctionConstant {
    #[error("delete the `const` keyword")]
    pub span: (Span, usize),
}

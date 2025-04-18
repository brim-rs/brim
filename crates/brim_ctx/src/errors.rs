use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;

#[derive(Diagnostic)]
#[error("entrypoint file: '{file}' doesn't contain a main function")]
pub struct NoMainFunction {
    pub file: String,
}

#[derive(Diagnostic)]
#[error("found item with name `main` but it is not a function")]
pub struct MainFunctionNotFunction {
    #[error("delete the `main` item")]
    pub span: (Span, usize),
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

#[derive(Diagnostic)]
#[error("symbol `{name}` not found in module `{module}`")]
pub struct SymbolNotFound {
    #[error("symbol `{name}` not found")]
    pub span: (Span, usize),
    pub name: String,
    pub module: String,
}

#[derive(Diagnostic)]
#[error("symbol `{name}` imported from module `{module}` found but is private")]
pub struct SymbolPrivate {
    #[error("imported here")]
    pub imported: (Span, usize),
    #[error("defined here")]
    pub defined: (Span, usize),
    pub name: String,
    pub module: String,
    #[note]
    pub note: &'static str,
}

use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;

#[derive(Diagnostic)]
#[error("undeclared variable `{name}`")]
pub struct UndeclaredVariable {
    #[error("variable `{name}` is not declared in this scope")]
    pub span: (Span, usize),
    pub name: String,
}

#[derive(Diagnostic)]
#[error("call to undeclared function `{name}`")]
pub struct UndeclaredFunction {
    #[error("function `{name}` is not declared in this scope")]
    pub span: (Span, usize),
    pub name: String,
}

#[derive(Diagnostic)]
#[error("attempted to access variable from outside comptime scope")]
pub struct AccessOutsideComptime {
    #[error("variable `{name}` is not accessible outside comptime scope")]
    pub span: (Span, usize),
    #[notel("declared here")]
    pub decl: (Span, usize),
    pub name: String,
}

#[derive(Diagnostic)]
#[error("struct `{name}` is not declared in this scope")]
pub struct UndeclaredStruct {
    #[error("struct `{name}` is not declared in this scope")]
    pub span: (Span, usize),
    pub name: String,
}

#[derive(Diagnostic)]
#[error("invalid path access. no struct or module named `{name}`")]
pub struct InvalidPathAccess {
    #[error("no struct or module named `{name}`")]
    pub span: (Span, usize),
    pub name: String,
}

#[derive(Diagnostic)]
#[error("namespace `{name}` doesn't have a symbol named `{symbol}`")]
pub struct NamespaceMissingSymbol {
    #[error("namespace `{name}` doesn't have a symbol named `{symbol}`")]
    pub span: (Span, usize),
    pub name: String,
    pub symbol: String,
}

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
    pub name: String,
}

#[derive(Diagnostic)]
#[error("builtin function expected {expected} arguments, found {found}")]
pub struct BuiltinFunctionArgCount {
    #[error("expected {expected} arguments")]
    pub span: (Span, usize),
    pub expected: usize,
    pub found: usize,
}

#[derive(Diagnostic)]
#[error("found duplicate initializer for field `{name}` in struct")]
pub struct DuplicateFieldInitializer {
    #[error("first initializer for field `{name}`")]
    pub first: (Span, usize),
    #[error("second initializer for field `{name}`")]
    pub second: (Span, usize),
    pub name: String,
}

#[derive(Diagnostic)]
#[error("found an item with a same name as the variant: `{name}`")]
pub struct DuplicateVariantName {
    #[error("variant defined here")]
    pub span: (Span, usize),
    #[error("duplicate found here")]
    pub dup: (Span, usize),
    pub name: String,
}

#[derive(Diagnostic)]
#[error("result or option types are not allowed inside extern function declarations")]
pub struct ExternFunctionResultOption {
    #[error("result or option types are not allowed inside extern function declarations")]
    pub span: (Span, usize),
}

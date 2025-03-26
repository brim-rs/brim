use crate::ty::HirTyKind;
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;

#[derive(Diagnostic)]
#[error("cannot initialize variable `{name}` of type `{ty}` with value of type `{val_ty}`")]
pub struct CannotInitializeVariable {
    #[error]
    pub span: (Span, usize),
    pub name: String,
    pub ty: HirTyKind,
    pub val_ty: HirTyKind,
}

#[derive(Diagnostic)]
#[error("function parameter `{name}` expected value of type `{expected}`, found `{found}`")]
pub struct FunctionParameterTypeMismatch {
    #[error]
    pub span: (Span, usize),
    pub name: String,
    pub expected: HirTyKind,
    pub found: HirTyKind,
}

#[derive(Diagnostic)]
#[error("function `{name}` expected return type of `{expected}`, found `{found}`")]
pub struct FunctionReturnTypeMismatch {
    #[error]
    pub span: (Span, usize),
    pub name: String,
    pub expected: HirTyKind,
    pub found: HirTyKind,
}

#[derive(Diagnostic)]
#[error("field `{field}` expected type `{expected}`, found `{found}`")]
pub struct FieldMismatch {
    #[error]
    pub span: (Span, usize),
    pub field: String,
    pub expected: HirTyKind,
    pub found: HirTyKind,
}

#[derive(Diagnostic)]
#[error("cannot assign a value of type `{found}` to a variable of type `{expected}`")]
pub struct AssignMismatch {
    #[error]
    pub span: (Span, usize),
    pub expected: HirTyKind,
    pub found: HirTyKind,
}

#[derive(Diagnostic)]
#[error("cannot assign to immutable variable `{name}`")]
pub struct CannotAssignToImmutable {
    #[error]
    pub span: (Span, usize),
    pub name: String,
}

#[derive(Diagnostic)]
#[error("function `{name}` does not return a value. expected value of type `{expected}`")]
pub struct NoReturnFound {
    #[error]
    pub span: (Span, usize),
    pub name: String,
    pub expected: HirTyKind,
}

#[derive(Diagnostic)]
#[error("variables can't be initialized or assigned to with an expression of type void")]
pub struct CannotInitializeWithVoid {
    #[error]
    pub span: (Span, usize),
}

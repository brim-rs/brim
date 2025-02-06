use crate::ty::HirTyKind;
use brim_ast::expr::{BinOpKind, UnaryOp};
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
use crate::ty::HirTyKind;
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;

#[derive(Diagnostic)]
#[error(
    "comptime expression expected to evaluate to a primitive: `{expected_ty}`, but found `{actual_ty}`"
)]
pub struct ComptimeExprExpectedTy {
    #[error]
    pub span: (Span, usize),
    pub expected_ty: HirTyKind,
    pub actual_ty: HirTyKind,
}

#[derive(Diagnostic)]
#[error("no value found returned from comptime expression")]
pub struct NoValueReturned {
    #[error]
    pub span: (Span, usize),
}

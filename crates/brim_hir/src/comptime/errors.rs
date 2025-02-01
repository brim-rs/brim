
use brim_ast::expr::{BinOpKind, UnaryOp};
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;
use crate::ty::HirTyKind;

#[derive(Diagnostic)]
#[error("comptime expression expected to evaluate to a primitive: `{expected_ty}`, but found `{actual_ty}`")]
pub struct ComptimeExprExpectedTy {
    #[error]
    pub span: (Span, usize),
    pub expected_ty: HirTyKind,
    pub actual_ty: HirTyKind,
}
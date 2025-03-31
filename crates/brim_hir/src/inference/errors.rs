use crate::ty::HirTyKind;
use brim_ast::expr::{BinOpKind, UnaryOp};
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;

#[derive(Diagnostic)]
#[error("cannot apply unary operator `{op}` to type `{ty}`")]
pub struct CannotApplyUnary {
    #[error]
    pub span: (Span, usize),
    pub op: UnaryOp,
    pub ty: HirTyKind,
}

#[derive(Diagnostic)]
#[error("cannot apply binary operator `{op}` to types `{lhs}` and `{rhs}`")]
pub struct CannotApplyBinary {
    #[error]
    pub span: (Span, usize),
    pub op: BinOpKind,
    pub lhs: HirTyKind,
    pub rhs: HirTyKind,
}

#[derive(Diagnostic)]
#[error("cannot logically compare types `{lhs}` and `{rhs}` using `{op}`")]
pub struct CannotCompare {
    #[error]
    pub span: (Span, usize),
    pub op: BinOpKind,
    pub lhs: HirTyKind,
    pub rhs: HirTyKind,
}

#[derive(Diagnostic)]
#[error("no method `{method}` found for type `{ty}`")]
pub struct NoMethod {
    #[error]
    pub span: (Span, usize),
    pub method: String,
    pub ty: HirTyKind,
}

#[derive(Diagnostic)]
#[error("`orelse` operator can only be applied to option types, found `{ty}`")]
pub struct OrelseExpectedOption {
    #[error]
    pub span: (Span, usize),
    pub ty: HirTyKind,
}

#[derive(Diagnostic)]
#[error("invalid argument count to a function. expected `{expected}` but found `{found}`")]
pub struct InvalidFunctionArgCount {
    #[error]
    pub span: (Span, usize),
    pub expected: u16,
    pub found: u16,
}

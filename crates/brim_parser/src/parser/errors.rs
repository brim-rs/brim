use brim_ast::token::{AssignOpToken, TokenKind};
use brim_diagnostics::diagnostic::ToDiagnostic;
use brim_diagnostics::diagnostic::Label;
use brim_diagnostics::diagnostic::LabelStyle;
use brim_diagnostics::diagnostic::Severity;
use brim_diag_macro::Diagnostic;
use brim_span::span::Span;

#[derive(Diagnostic)]
#[error("Invalid function signature. {message}")]
pub struct InvalidFunctionSignature {
    pub(crate) message: String,
    #[error("invalid signature")]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("Invalid modifier order. {message}")]
pub struct InvalidModifierOrder {
    pub(crate) message: String,
    #[error("invalid modifier")]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("expected identifier. {message}")]
pub struct ExpectedIdentifier {
    pub(crate) message: String,
    #[error]
    pub span: (Span, usize),
}

// TODO: use `add` label to show `>` in the diagnostic
#[derive(Diagnostic)]
#[error("expected closing `>` for generics.")]
pub struct ExpectedClosingGenerics {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("expected `{expected}`, but found `{found}`.")]
pub struct ExpectedToken {
    pub expected: TokenKind,
    pub found: TokenKind,
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("`const` should be placed before reference of pointer.")]
pub struct ConstAfter {
    #[error("move `const` before {before}")]
    pub span: (Span, usize),
    pub before: &'static str,
}

#[derive(Diagnostic)]
#[error("couldn't find parameter list for function.")]
pub struct MissingParamList {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[warning("unnecessary `self` keyword. `self` is always available in methods.")]
pub struct UnnecessarySelf {
    #[warning("argument will be ignored")]
    pub span: (Span, usize),
}

// TODO: consider changing to something like: "`self` not allowed as a parameter name."
#[derive(Diagnostic)]
#[error("`self` keyword is only available in methods.")]
pub struct SelfOutsideMethod {
    #[error]
    pub span: (Span, usize),
    #[note]
    pub note: &'static str,
}

#[derive(Diagnostic)]
#[error("only trait methods can have empty body.")]
pub struct EmptyBody {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("variables can be only initialized with `=`, found `{found}`.")]
pub struct InvalidVariableInit {
    pub found: AssignOpToken,
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("found unexpected token `{found}`.")]
pub struct UnexpectedToken {
    pub found: TokenKind,
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("`else` branch can't have an expression.")]
pub struct ElseBranchExpr {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("found `else if` after `else` block.")]
pub struct ElseIfAfterElse {
    #[error("here is the `else if` block")]
    pub else_if: (Span, usize),
    #[error("here is the `else` block")]
    pub else_block: (Span, usize),
}
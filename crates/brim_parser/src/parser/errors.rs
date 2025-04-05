use brim_ast::token::{AssignOpToken, Lit, TokenKind};
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
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
#[error("couldn't find parameter list for function.")]
pub struct MissingParamList {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("`self` keyword is only available in methods.")]
pub struct SelfOutsideMethod {
    #[error]
    pub span: (Span, usize),
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

#[derive(Diagnostic)]
#[error("use statement requires braces in the item list.")]
pub struct UseStatementBraces {
    #[error("add braces around the item list")]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("missing `from` keyword after item list.")]
pub struct MissingFromKeyword {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("unexpected suffix for literal: `{lit}`.")]
pub struct UnexpectedLiteralSuffix {
    pub lit: Lit,
    #[error]
    pub span: (Span, usize),
    #[note]
    pub note: String,
}

#[derive(Diagnostic)]
#[error("found invalid suffix for literal: `{lit}`.")]
pub struct InvalidLiteralSuffix {
    pub lit: Lit,
    #[error]
    pub span: (Span, usize),
    #[note]
    pub note: String,
}

#[derive(Diagnostic)]
#[error("unknown item. found `{found}`.")]
pub struct UnknownItem {
    pub found: TokenKind,
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("unexpected token in match expression. found `{found}`.")]
pub struct UnexpectedTokenInMatch {
    pub found: TokenKind,
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("multiple else arms are not allowed in match expression.")]
pub struct MultipleElseArmsError {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("match expression must have at least one arm.")]
pub struct EmptyMatchExpressionError {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("expected fat arrow `=>` in match arm, found `{found}`.")]
pub struct MissingFatArrowError {
    pub found: TokenKind,
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("invalid pattern in match arm.")]
pub struct InvalidMatchPatternError {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("extern block only accepts function and type alias items. found `{found}`.")]
pub struct InvalidExternBlockItem {
    #[error]
    pub span: (Span, usize),
    pub found: String,
}

#[derive(Diagnostic)]
#[error("expected string literal.")]
pub struct ExpectedStringLiteralError {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("if are not allowed as an expression. use ternary operator instead.")]
pub struct IfAsExpressionError {
    #[error]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("expected `:` in a ternary operator.")]
pub struct ExpectedColonInTernaryError {
    #[error]
    pub span: (Span, usize),
}

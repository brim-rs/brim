use brim_ast::expr::{BinOpKind, UnaryOp};
use brim_ast::item::Ident;
use brim_ast::token::Lit;
use brim_ast::ty::Ty;
use brim_span::span::Span;
use crate::HirId;
use crate::stmts::HirStmt;

#[derive(Clone, Debug)]
pub struct HirExpr {
    pub id: HirId,
    pub kind: HirExprKind,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub enum HirExprKind {
    /// Binary operations with desugared operands.
    Binary(Box<HirExpr>, BinOpKind, Box<HirExpr>),
    /// Unary operations.
    Unary(UnaryOp, Box<HirExpr>),
    /// Field access: `x.name`.
    Field(Box<HirExpr>, Ident),
    /// Array indexing: `x[0]`.
    Index(Box<HirExpr>, Box<HirExpr>),
    /// Literal values like numbers or strings.
    Literal(Lit),
    /// Parenthesized expression: `(x)`.
    Paren(Box<HirExpr>),
    /// Variable reference.
    Var(Ident),
    /// Assignment.
    Assign(Box<HirExpr>, Box<HirExpr>),
    /// Conditionals desugared into a single structure.
    If {
        condition: Box<HirExpr>,
        then_branch: Box<HirExpr>,
        else_branch: Option<Box<HirExpr>>,
    },
    /// Function calls.
    Call(Box<HirExpr>, Vec<HirExpr>),
    /// Block of statements or expressions.
    Block(Vec<HirStmt>),
    /// Return statement: `return x`.
    Return(Box<HirExpr>),
}


/// Desugared condition branches for `if` or `else if`.
#[derive(Clone, Debug)]
pub struct ConditionBranch {
    pub condition: Box<HirExpr>,
    pub block: Box<HirExpr>,
}

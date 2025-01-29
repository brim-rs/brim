use crate::{HirId, stmts::HirStmt, ty::HirTyKind};
use brim_ast::{
    expr::{BinOpKind, ConditionBranch, Expr, UnaryOp},
    item::Ident,
    token::Lit,
};
use brim_span::span::Span;

#[derive(Clone, Debug)]
pub struct HirExpr {
    pub id: HirId,
    pub kind: HirExprKind,
    pub span: Span,
    pub ty: HirTyKind,
}

impl HirExpr {
    pub fn as_ident(&self) -> Option<&Ident> {
        match &self.kind {
            HirExprKind::Var(ident) => Some(ident),
            _ => None,
        }
    }
}

// We no longer need parenthesized expressions, because the tree defines the structure.
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
    /// Variable reference.
    Var(Ident),
    /// Assignment.
    Assign(Box<HirExpr>, Box<HirExpr>),
    /// Conditionals desugared into a single structure.
    If(HirIfExpr),
    /// Function calls.
    Call(Box<HirExpr>, Vec<HirExpr>),
    /// Block of statements or expressions.
    Block(HirBlock),
    /// Return statement: `return x`.
    Return(Box<HirExpr>),
}

#[derive(Clone, Debug)]
pub struct HirIfExpr {
    pub span: Span,
    pub condition: Box<HirExpr>,
    pub then_block: Box<HirExpr>,
    pub else_block: Option<Box<HirExpr>>,
    pub else_ifs: Vec<HirConditionBranch>,
}

#[derive(Clone, Debug)]
pub struct HirBlock {
    pub id: HirId,
    pub span: Span,
    pub stmts: Vec<HirStmt>,
}

#[derive(Clone, Debug)]
pub struct HirConditionBranch {
    pub condition: Box<HirExpr>,
    pub block: Box<HirExpr>,
}

#[derive(Clone, Debug)]
pub struct HirConstExpr {
    pub id: HirId,
    pub span: Span,
    pub body: HirId,
}

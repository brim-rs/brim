use crate::{
    expr::{HirExpr, HirIfStmt},
    ty::HirTyKind,
};
use brim_ast::{ItemId, item::Ident};
use brim_span::span::Span;

#[derive(Clone, Debug)]
pub struct HirStmt {
    pub id: ItemId,
    pub kind: HirStmtKind,
    pub span: Span,
}

impl HirStmt {
    pub fn can_be_used_for_inference(&self) -> Option<HirTyKind> {
        match &self.kind {
            // Let statement can't be used for inference.
            HirStmtKind::Let { .. } => None,
            HirStmtKind::Expr(expr) => Some(expr.ty.clone()),
            HirStmtKind::If(_) => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum HirStmtKind {
    /// `let x: i32 = 5;`
    Let { ident: Ident, ty: Option<HirTyKind>, value: Option<HirExpr> },
    /// An expression statement.
    Expr(HirExpr),
    /// An if statement.
    If(HirIfStmt),
}

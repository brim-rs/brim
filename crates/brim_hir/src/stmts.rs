use crate::{HirId, expr::HirExpr, ty::HirTy};
use brim_ast::item::Ident;
use brim_span::span::Span;
use crate::ty::HirTyKind;

#[derive(Clone, Debug)]
pub struct HirStmt {
    pub id: HirId,
    pub kind: HirStmtKind,
    pub span: Span,
}

impl HirStmt {
    pub fn can_be_used_for_inference(&self) -> Option<HirTyKind> {
        match &self.kind {
            // Let statement can't be used for inference.
            HirStmtKind::Let { .. } => None,
            HirStmtKind::Expr(expr) => Some(expr.ty.clone()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum HirStmtKind {
    /// `let x: i32 = 5;`
    Let {
        ident: Ident,
        ty: Option<HirTyKind>,
        value: Option<HirExpr>,
    },
    /// An expression statement.
    Expr(HirExpr),
}

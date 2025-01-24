use brim_ast::item::Ident;
use brim_span::span::Span;
use crate::expr::HirExpr;
use crate::HirId;
use crate::ty::HirTy;

#[derive(Clone, Debug)]
pub struct HirStmt {
    pub id: HirId,
    pub kind: HirStmtKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum HirStmtKind {
    /// `let x: i32 = 5;`
    Let {
        ident: Ident,
        ty: Option<HirTy>,
        value: Option<HirExpr>,
    },
    /// An expression statement.
    Expr(HirExpr),
}

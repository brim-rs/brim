use crate::NodeId;
use brim_span::span::Span;
use crate::expr::Expr;
use crate::item::{Ident, Item};
use crate::ty::Ty;

#[derive(Clone, Debug)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Let(Let),
    Item(Item),
    Expr(Expr)
}

#[derive(Clone, Debug)]
pub struct Let {
    pub id: NodeId,
    pub span: Span,
    pub ty: Option<Ty>,
    pub ident: Ident,
    // E.g. `let x: i32 = 5;` or `let x;`
    pub value: Option<Expr>,
}

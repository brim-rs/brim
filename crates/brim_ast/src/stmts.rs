use crate::{
    NodeId,
    expr::Expr,
    item::{Ident, Item},
    ty::Ty,
};
use brim_span::span::Span;

#[derive(Clone, Debug)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Let(Let),
    Expr(Expr),
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

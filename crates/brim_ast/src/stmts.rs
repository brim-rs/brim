use crate::{
    ItemId,
    expr::{Expr, IfExpr},
    item::Ident,
    ty::Ty,
};
use brim_span::span::Span;

#[derive(Clone, Debug)]
pub struct Stmt {
    pub id: ItemId,
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Let(Let),
    Expr(Expr),
    If(Expr),
}

#[derive(Clone, Debug)]
pub struct Let {
    pub id: ItemId,
    pub span: Span,
    pub ty: Option<Ty>,
    pub ident: Ident,
    // E.g. `let x: i32 = 5;` or `let x;`
    pub value: Option<Expr>,
}

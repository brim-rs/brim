use crate::{
    ItemId,
    expr::{Expr, Match},
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
pub struct IfStmt {
    pub span: Span,
    pub condition: Box<Expr>,
    pub then_block: Box<Expr>,
    pub else_block: Option<Box<Expr>>,
    pub else_ifs: Vec<ConditionBranch>,
}

#[derive(Clone, Debug)]
pub struct ConditionBranch {
    pub span: Span,
    pub condition: Box<Expr>,
    pub block: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    Let(Let),
    Expr(Expr),
    If(IfStmt),
    Match(Match),
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

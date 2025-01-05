use brim_span::span::Span;
use crate::NodeId;

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,

}
#[derive(Clone, Debug)]
pub enum ExprKind {}

#[derive(Clone, Debug)]
pub struct ConstExpr {
    pub id: NodeId,
    pub expr: Box<Expr>,
}
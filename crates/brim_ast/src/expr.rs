use crate::NodeId;
use brim_span::span::Span;
use crate::item::{Block, Ident};
use crate::token::{AssignOpToken, Lit};

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
}
#[derive(Clone, Debug)]
pub enum ExprKind {
    /// `1 + 2`, `x * y`
    Binary(Box<Expr>, BinOpKind, Box<Expr>),
    /// `try x()`, `!x`
    Unary(UnaryOp, Box<Expr>),
    /// `x.name`
    Field(Box<Expr>, Ident),
    /// `x[0]`
    Index(Box<Expr>, Box<Expr>),
    /// `123`, `"hello"`, etc.
    Literal(Lit),
    /// `(x + y) * z`
    Paren(Box<Expr>),
    /// `return x`
    Return(Box<Expr>),
    /// `x`
    Var(Ident),
    /// `x += 1`, `x *= 2`
    AssignOp(Box<Expr>, AssignOpToken, Box<Expr>),
    /// `x = 1`, `y = x`
    Assign(Box<Expr>, Box<Expr>),
    /// `if x { y } else { z }`
    If(IfExpr),
    /// `{ ... }`
    Block(Block),
    /// `func(x, y)`
    Call(Box<Expr>, Vec<Expr>),
}

#[derive(Clone, Debug)]
pub struct IfExpr {
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
pub enum UnaryOp {
    /// `-`
    Minus,
    /// `!`
    Not,
    /// `*`
    Deref,
    /// `try x()`
    Try,
}

#[derive(Clone, Debug)]
pub enum BinOpKind {
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `==`
    EqEq,
    /// `!=`
    Ne,
    /// `>=`
    Ge,
    /// `>`
    Gt,
    /// `&&`
    AndAnd,
    /// `||`
    OrOr,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Multiply,
    /// `**`
    Power,
    /// `/`
    Divide,
    /// `%`
    Modulo,
    /// `^`
    Caret,
    /// `&`
    And,
    /// `|`
    Or,
    /// `<<`
    ShiftLeft,
    /// `>>`
    ShiftRight,
}

/// Enum representing the associativity of a binary operator.
#[derive(Debug, Clone, PartialEq)]
pub enum BinOpAssociativity {
    /// Left-associative operators group from the left.
    Left,
    /// Right-associative operators group from the right.
    Right,
}

impl BinOpKind {
    pub fn associativity(&self) -> BinOpAssociativity {
        match self {
            BinOpKind::Power => BinOpAssociativity::Right,
            _ => BinOpAssociativity::Left,
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            // Highest precedence
            BinOpKind::Power => 20,
            BinOpKind::Multiply | BinOpKind::Divide | BinOpKind::Modulo => 19,
            BinOpKind::Plus | BinOpKind::Minus => 18,
            BinOpKind::ShiftLeft | BinOpKind::ShiftRight => 17,
            BinOpKind::And => 16,
            BinOpKind::Caret => 15,
            BinOpKind::Or => 14,
            // Relational operators
            BinOpKind::Lt
            | BinOpKind::Le
            | BinOpKind::Gt
            | BinOpKind::Ge => 13,
            // Equality operators
            BinOpKind::EqEq | BinOpKind::Ne => 12,
            // Logical operators
            BinOpKind::AndAnd => 11,
            BinOpKind::OrOr => 10,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ConstExpr {
    pub id: NodeId,
    pub expr: Box<Expr>,
}

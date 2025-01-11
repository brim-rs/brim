use crate::NodeId;
use brim_span::span::Span;
use crate::item::Ident;
use crate::token::Lit;

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
    Literal(Lit)
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

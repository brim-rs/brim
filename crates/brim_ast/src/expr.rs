use crate::{
    ItemId,
    item::{Block, GenericArgs, Ident},
    token::{AssignOpToken, Lit},
    ty::Ty,
};
use brim_span::span::Span;
use indexmap::IndexMap;
use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: ItemId,
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn as_ident(&self) -> Option<&Ident> {
        match &self.kind {
            ExprKind::Var(ident) => Some(ident),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    /// `[1, 2, 3]`
    Array(Vec<Expr>),
    /// `1 + 2`, `x * y`
    Binary(Box<Expr>, BinOpKind, Box<Expr>),
    /// `try x()`, `!x`
    Unary(Span, UnaryOp, Box<Expr>),
    /// `x.name`
    Field(Vec<Ident>),
    /// `x[0]`
    Index(Box<Expr>, Box<Expr>),
    /// `x.foo()`
    MethodCall(Vec<Ident>, Box<Expr>),
    /// `Vec::new()`
    StaticAccess(Vec<Ident>, Box<Expr>),
    /// `123`, `"hello"`, etc.
    Literal(Lit, Span),
    /// `(x + y) * z`
    Paren(Box<Expr>),
    /// `return x`
    Return(Box<Expr>, Span),
    /// `x`
    Var(Ident),
    /// `x += 1`, `x *= 2`
    AssignOp(Box<Expr>, AssignOpToken, Box<Expr>),
    /// `x = 1`, `y = x`
    Assign(Box<Expr>, Box<Expr>),
    /// `{ ... }`
    Block(Block),
    /// `func(x, y)`.
    Call(Box<Expr>, Vec<Expr>),
    /// `@ok`, `@err` builtins
    Builtin(Ident, Vec<Expr>),
    /// `comptime { ... }`
    Comptime(Box<Expr>),
    /// `Vec2 { x: 1, y: 2 }`
    StructConstructor(Ident, GenericArgs, IndexMap<Ident, Expr>),
    /// `match x { ... }`
    Match(Match),
    /// `windows::io::File`
    Path(Vec<Ident>),
    /// `i32`. Type as an value.
    Type(Box<Ty>),
    /// `expr?` unwrap an option
    Unwrap(Box<Expr>),
    /// `cond ? then : else`
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct Match {
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum MatchArm {
    /// `case 1 => { ... }`
    Case(Expr, Expr),
    /// `else => { ... }`
    Else(Expr),
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
    /// `&x`
    Ref,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Minus => write!(f, "- (numeric negation)"),
            UnaryOp::Not => write!(f, "! (logical negation)"),
            UnaryOp::Deref => write!(f, "* (dereference)"),
            UnaryOp::Try => write!(f, "try (try operator)"),
            UnaryOp::Ref => write!(f, "& (reference)"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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
    /// `orelse`
    OrElse,
}

impl Display for BinOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOpKind::Lt => write!(f, "< (less than)"),
            BinOpKind::Le => write!(f, "<= (less than or equal)"),
            BinOpKind::EqEq => write!(f, "== (equal)"),
            BinOpKind::Ne => write!(f, "!= (not equal)"),
            BinOpKind::Ge => write!(f, ">= (greater than or equal)"),
            BinOpKind::Gt => write!(f, "> (greater than)"),
            BinOpKind::AndAnd => write!(f, "&& (logical and)"),
            BinOpKind::OrOr => write!(f, "|| (logical or)"),
            BinOpKind::Plus => write!(f, "+ (addition)"),
            BinOpKind::Minus => write!(f, "- (subtraction)"),
            BinOpKind::Multiply => write!(f, "* (multiplication)"),
            BinOpKind::Power => write!(f, "** (exponentiation)"),
            BinOpKind::Divide => write!(f, "/ (division)"),
            BinOpKind::Modulo => write!(f, "% (modulo)"),
            BinOpKind::Caret => write!(f, "^ (bitwise xor)"),
            BinOpKind::And => write!(f, "& (bitwise and)"),
            BinOpKind::Or => write!(f, "| (bitwise or)"),
            BinOpKind::ShiftLeft => write!(f, "<< (bitwise shift left)"),
            BinOpKind::ShiftRight => write!(f, ">> (bitwise shift right)"),
            BinOpKind::OrElse => write!(f, "orelse (logical or else)"),
        }
    }
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
            BinOpKind::Lt | BinOpKind::Le | BinOpKind::Gt | BinOpKind::Ge => 13,
            // Equality operators
            BinOpKind::EqEq | BinOpKind::Ne => 12,
            // Logical operators
            BinOpKind::AndAnd => 11,
            BinOpKind::OrOr => 10,
            BinOpKind::OrElse => 9,
        }
    }
}

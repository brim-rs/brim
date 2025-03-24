use crate::{
    comptime::ComptimeReturnValue,
    items::{HirCallParam, HirGenericArgs},
    stmts::HirStmt,
    ty::HirTyKind,
};
use brim_ast::{
    ItemId,
    expr::{BinOpKind, UnaryOp},
    item::Ident,
    token::Lit,
};
use brim_span::span::Span;
use indexmap::IndexMap;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct HirExpr {
    pub id: ItemId,
    pub kind: HirExprKind,
    pub span: Span,
    pub ty: HirTyKind,
}

impl HirExpr {
    pub fn as_ident(&self) -> Option<&Ident> {
        match &self.kind {
            HirExprKind::Var(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn as_block(&self) -> Option<HirBlock> {
        match &self.kind {
            HirExprKind::Block(block) => Some(block.clone()),
            _ => None,
        }
    }

    pub fn as_call(&self) -> &HirExpr {
        match &self.kind {
            HirExprKind::Call(expr, _, _) => expr,
            _ => panic!("Expected call expression"),
        }
    }
}

// We no longer need parenthesized expressions, because the tree defines the structure.
#[derive(Clone, Debug)]
pub enum HirExprKind {
    /// Array literals: `[1, 2, 3]`.
    Array(Vec<HirExpr>),
    /// Binary operations with desugared operands.
    Binary(Box<HirExpr>, BinOpKind, Box<HirExpr>),
    /// Unary operations.
    Unary(UnaryOp, Box<HirExpr>),
    /// Field access: `x.name`.
    Field(Box<HirExpr>, Ident),
    /// Array indexing: `x[0]`.
    Index(Box<HirExpr>, Box<HirExpr>),
    /// Literal values like numbers or strings.
    Literal(Lit),
    /// Variable reference.
    Var(Ident),
    /// Assignment.
    Assign(Box<HirExpr>, Box<HirExpr>),
    /// Conditionals desugared into a single structure.
    If(HirIfExpr),
    /// Function calls.
    Call(Box<HirExpr>, Vec<HirExpr>, Vec<HirCallParam>),
    /// Block of statements or expressions.
    Block(HirBlock),
    /// Return statement: `return x`.
    Return(Box<HirExpr>),
    /// Built-in functions.
    Builtin(Ident, Vec<HirExpr>),
    /// Struct constructor.
    StructConstructor(HirStructConstructor),
    /// Match expressions.
    Match(Box<HirExpr>, Vec<HirMatchArm>),
    /// Path to a module or item.
    Path(ItemId),
    /// Type as a value
    Type(HirTyKind),
    /// Comptime block to be evaluated at compile time.
    Comptime(ComptimeValue),
    /// Static access to a struct field.
    StaticAccess(ItemId, Box<HirExpr>),
    /// Method call on an expression.
    MethodCall(Ident, Box<HirExpr>),
}

impl HirExprKind {
    pub fn dummy() -> Self {
        HirExprKind::Var(Ident::dummy())
    }
}

#[derive(Clone, Debug)]
/// it can be either resolved value or a expr to be evaluated
pub enum ComptimeValue {
    Resolved(ComptimeReturnValue),
    Expr(Box<HirExpr>),
}

impl ComptimeValue {
    pub fn resolved(&self) -> &ComptimeReturnValue {
        match self {
            ComptimeValue::Resolved(val) => val,
            _ => panic!("Expected resolved value"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum HirMatchArm {
    /// `case 1 => { ... }`
    Case(HirExpr, HirExpr),
    /// `else => { ... }`
    Else(HirExpr),
}

#[derive(Clone, Debug)]
pub struct HirStructConstructor {
    pub id: ItemId,
    pub name: Ident,
    pub generics: HirGenericArgs,
    pub fields: IndexMap<Ident, HirExpr>,
    pub field_types: HashMap<Ident, HirTyKind>,
}

#[derive(Clone, Debug)]
pub struct HirIfExpr {
    pub span: Span,
    pub condition: Box<HirExpr>,
    pub then_block: Box<HirExpr>,
    pub else_block: Option<Box<HirExpr>>,
    pub else_ifs: Vec<HirConditionBranch>,
}

#[derive(Clone, Debug)]
pub struct HirBlock {
    pub id: ItemId,
    pub span: Span,
    pub stmts: Vec<HirStmt>,
}

#[derive(Clone, Debug)]
pub struct HirConditionBranch {
    pub condition: Box<HirExpr>,
    pub block: Box<HirExpr>,
}

#[derive(Clone, Debug)]
pub struct HirConstExpr {
    pub id: ItemId,
    pub span: Span,
    pub body: ItemId,
}

use std::fmt::Debug;
use std::path::Path;
use brim_span::span::Span;
use brim_span::symbols::Symbol;
use crate::expr::Expr;
use crate::NodeId;
use crate::stmts::Stmt;
use crate::ty::{Const, Ty};

#[derive(Clone, Debug)]
pub struct Item {
    pub id: NodeId,
    pub span: Span,
    pub vis: Visibility,
    pub ident: Ident,
    pub kind: ItemKind,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.name)
    }
}

#[derive(Clone, Debug)]
pub struct Visibility {
    pub span: Span,
    pub kind: VisibilityKind,
}

#[derive(Clone, Debug)]
pub enum VisibilityKind {
    Public,
    Private,
}

impl VisibilityKind {
    pub fn is_public(&self) -> bool {
        matches!(self, VisibilityKind::Public)
    }
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    /// Function declaration
    Fn(FnDecl),
}

#[derive(Clone, Debug)]
pub struct FnDecl {
    pub sig: FnSignature,
    pub generics: Generics,
    /// Allowed to be empty for trait functions
    pub body: Option<Block>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub id: NodeId,
    pub span: Span,
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct Generics {
    pub span: Span,
    pub params: Vec<GenericParam>,
}

#[derive(Clone, Debug)]
pub struct GenericParam {
    pub id: NodeId,
    pub ident: Ident,
    pub kind: GenericKind,
}

#[derive(Clone, Debug)]
pub enum GenericKind {
    Type {
        default: Option<Ty>,
    },
    NonType {
        default: Option<Expr>,
        ty: Ty,
    },
}

#[derive(Clone, Debug)]
pub struct FnSignature {
    /// `const fn ...` (brim) -> `constexpr ...` (C++)
    pub constant: Const,
    pub name: Ident,
    pub return_type: FnReturnType,
    pub params: Vec<Param>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub ty: Ty,
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum FnReturnType {
    /// `-> T` (brim) -> `T ...` (C++)
    Ty(Ty),
    /// `-> void` or empty type (brim) -> `void` (C++)
    Default,
}
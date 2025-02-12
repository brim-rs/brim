use crate::{
    Break, Empty, NodeId, While,
    expr::Expr,
    stmts::Stmt,
    ty::{Const, Ty},
};
use brim_span::{span::Span, symbols::Symbol};
use std::fmt::{Debug, Display};

#[derive(Clone, Debug)]
pub struct Item {
    pub id: NodeId,
    pub span: Span,
    pub vis: Visibility,
    pub ident: Ident,
    pub kind: ItemKind,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }

    pub fn is_reserved(&self) -> bool {
        self.name >= Break && self.name <= While
    }

    pub fn dummy() -> Self {
        Self {
            name: Empty,
            span: Span::initial(),
        }
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.name)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug)]
pub struct Visibility {
    pub span: Span,
    pub kind: VisibilityKind,
}

impl Visibility {
    pub fn from_bool(is_public: bool, span: Span) -> Self {
        let kind = if is_public {
            VisibilityKind::Public
        } else {
            VisibilityKind::Private
        };
        Self { span, kind }
    }
}

#[derive(Clone, Debug, PartialEq)]
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
    /// Use statement
    Use(Use),
    /// Struct declaration eg. `struct Foo { ... }`
    Struct(Struct),
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub span: Span,
    pub ident: Ident,
    pub fields: Vec<Field>,
    pub generics: Generics,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub id: NodeId,
    pub span: Span,
    pub ident: Ident,
    pub ty: Ty,
    pub vis: Visibility,
}

#[derive(Clone, Debug)]
pub struct Use {
    pub span: Span,
    pub path: Vec<PathItemKind>,
    pub imports: ImportsKind,
}

#[derive(Clone, Debug)]
pub enum PathItemKind {
    Parent,
    Module(Ident),
}

#[derive(Clone, Debug)]
pub enum ImportsKind {
    /// `use { foo, bar } from "test";`
    List(Vec<Ident>),
    /// `use * from "test";`
    All,
}

#[derive(Clone, Debug)]
pub struct FnDecl {
    pub sig: FnSignature,
    pub generics: Generics,
    /// Allowed to be empty for trait functions
    pub body: Option<Block>,
    pub context: FunctionContext,
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionContext {
    Item,
    Trait,
    Impl,
}

impl FunctionContext {
    pub fn allows_self(&self) -> bool {
        match self {
            FunctionContext::Item => false,
            _ => true,
        }
    }

    pub fn allows_empty_body(&self) -> bool {
        match self {
            FunctionContext::Trait => true,
            _ => false,
        }
    }
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
pub struct GenericArgs {
    pub span: Span,
    pub params: Vec<GenericArg>,
}

#[derive(Clone, Debug)]
pub struct GenericArg {
    pub id: NodeId,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub struct GenericParam {
    pub id: NodeId,
    pub ident: Ident,
    pub kind: GenericKind,
}

#[derive(Clone, Debug)]
pub enum GenericKind {
    Type { default: Option<Ty> },
    NonType { default: Option<Expr>, ty: Ty },
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
    pub name: Ident,
}

#[derive(Clone, Debug)]
pub enum FnReturnType {
    /// `-> T` (brim) -> `T ...` (C++)
    Ty(Ty),
    /// `-> void` or empty type (brim) -> `void` (C++)
    Default,
}

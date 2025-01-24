use brim_ast::item::{FnReturnType, Ident};
use brim_span::span::Span;
use crate::expr::HirConstExpr;
use crate::{HirId};
use crate::stmts::HirStmt;
use crate::ty::HirTy;

#[derive(Clone, Debug)]
pub struct HirItem {
    pub id: HirId,
    pub span: Span,
    pub ident: Ident,
    pub kind: HirItemKind,
    pub is_public: bool,
}

#[derive(Clone, Debug)]
pub enum HirItemKind {
    /// Function definition
    Fn(HirFn),
    /// Import statement
    Use(HirUse),
}

#[derive(Clone, Debug)]
pub struct HirUse {
    pub span: Span,
    pub path: Vec<Ident>,
    pub imports: HirImportsKind,
}

#[derive(Clone, Debug)]
pub enum HirImportsKind {
    /// `use { foo, bar } from "test";`
    List(Vec<Ident>),
    /// `use * from "test";`
    All,
}

#[derive(Clone, Debug)]
pub struct HirFn {
    pub sig: HirFnSig,
    /// ID of the function body block
    pub body: Option<HirId>,
}

#[derive(Clone, Debug)]
pub struct HirFnSig {
    pub constant: bool,
    pub name: Ident,
    /// We use option instead of FnReturnType
    pub return_type: Option<HirTy>,
    pub params: Vec<HirParam>,
    pub generics: HirGenerics,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct HirGenerics {
    pub params: Vec<HirGenericParam>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct HirParam {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub ty: HirTy,
}

#[derive(Clone, Debug)]
pub struct HirBlock {
    pub id: HirId,
    pub span: Span,
    pub stmts: Vec<HirStmt>,
}

#[derive(Clone, Debug)]
pub struct HirGenericParam {
    pub id: HirId,
    pub name: Ident,
    pub kind: HirGenericKind,
}

#[derive(Clone, Debug)]
pub enum HirGenericKind {
    Type { default: Option<HirTy> },
    Const { ty: HirTy, default: Option<HirConstExpr> },
}

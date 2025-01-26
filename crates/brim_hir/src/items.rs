use crate::{
    HirId,
    expr::HirConstExpr,
    ty::{HirTy, HirTyKind},
};
use brim_ast::{NodeId, item::Ident, ty::Ty};
use brim_span::span::Span;
use std::path::PathBuf;
use brim_ctx::GlobalSymbolId;

#[derive(Clone, Debug)]
pub struct HirItem {
    pub id: HirId,
    /// Only kept for the convenience of the transformation phase.
    pub old_sym_id: GlobalSymbolId,
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
    pub imports: HirImportsKind,
    pub resolved_path: PathBuf,
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
    /// Return type specified by the user or the default return type. Different from the signature return type.
    pub ret_type: HirTyKind,
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
pub struct HirGenericArgs {
    pub span: Span,
    pub params: Vec<HirGenericArg>,
}

#[derive(Clone, Debug)]
pub struct HirGenericArg {
    pub id: HirId,
    pub ty: HirTy,
}

#[derive(Clone, Debug)]
pub struct HirParam {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub ty: HirTy,
}

#[derive(Clone, Debug)]
pub struct HirGenericParam {
    pub id: HirId,
    pub name: Ident,
    pub kind: HirGenericKind,
}

#[derive(Clone, Debug)]
pub enum HirGenericKind {
    Type {
        default: Option<HirTy>,
    },
    Const {
        ty: HirTy,
        default: Option<HirConstExpr>,
    },
}

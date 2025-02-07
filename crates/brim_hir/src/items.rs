use crate::{
    HirId,
    expr::HirConstExpr,
    ty::{HirTy, HirTyKind},
};
use brim_ast::{
    item::Ident,
    token::{Lit, LitKind},
};
use brim_middle::{GlobalSymbolId, ModuleId};
use brim_span::span::Span;
use std::{fmt::Display, path::PathBuf};

#[derive(Clone, Debug)]
pub struct HirItem {
    pub id: HirId,
    /// Only kept for the convenience of the transformation phase.
    pub old_sym_id: GlobalSymbolId,
    pub span: Span,
    pub ident: Ident,
    pub kind: HirItemKind,
    pub is_public: bool,
    pub mod_id: ModuleId,
}

impl HirItem {
    pub fn as_fn(&self) -> &HirFn {
        match &self.kind {
            HirItemKind::Fn(f) => f,
            _ => panic!("Expected function item"),
        }
    }
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
    pub resolved_type: HirTyKind,
}

#[derive(Clone, Debug)]
pub struct HirFnSig {
    pub constant: bool,
    pub name: Ident,
    /// We use option instead of FnReturnType
    pub return_type: HirTyKind,
    pub params: HirFnParams,
    pub generics: HirGenerics,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct HirFnParams {
    pub span: Span,
    pub params: Vec<HirParam>,
}

#[derive(Clone, Debug)]
pub struct HirGenerics {
    pub params: Vec<HirGenericParam>,
    pub span: Span,
}

impl HirGenerics {
    pub fn is_generic(&self, ty: &HirTyKind) -> Option<HirGenericParam> {
        if let HirTyKind::Ident { ident, .. } = ty {
            self.params
                .iter()
                .find(|g| g.name.to_string() == *ident.to_string())
                .cloned()
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub struct HirGenericArgs {
    pub span: Span,
    pub params: Vec<HirGenericArg>,
}

impl Display for HirGenericArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<")?;
        for (i, param) in self.params.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param.ty.kind)?;
        }
        write!(f, ">")
    }
}

impl PartialEq for HirGenericArgs {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params
    }
}

impl HirGenericArgs {
    pub fn new(span: Span, params: Vec<HirGenericArg>) -> Self {
        Self { span, params }
    }

    pub fn empty() -> Self {
        Self {
            span: Span::DUMMY,
            params: vec![],
        }
    }
}

#[derive(Clone, Debug)]
pub struct HirGenericArg {
    pub id: HirId,
    pub ty: HirTy,
}

impl PartialEq for HirGenericArg {
    fn eq(&self, other: &Self) -> bool {
        self.ty.kind == other.ty.kind
    }
}

#[derive(Clone, Debug)]
pub struct HirParam {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub ty: HirTy,
}

#[derive(Clone, Debug)]
pub struct HirCallParam {
    pub span: Span,
    pub name: Ident,
    pub ty: HirTyKind,
    pub from_generic: Option<HirGenericParam>,
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
    Const { ty: HirTy, default: Option<Lit> },
}

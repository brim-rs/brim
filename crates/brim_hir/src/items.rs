use crate::{
    expr::ComptimeValue,
    ty::{HirTy, HirTyKind},
};
use brim_ast::{
    ItemId,
    item::{FunctionContext, Ident, Visibility},
    token::Lit,
};
use brim_middle::{GlobalSymbol, ModuleId};
use brim_span::{span::Span, symbols::Symbol};
use std::{collections::HashMap, fmt::Display, path::PathBuf};

#[derive(Clone, Debug)]
pub struct HirItem<Kind = HirItemKind> {
    pub id: ItemId,
    pub span: Span,
    pub ident: Ident,
    pub kind: Kind,
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

    pub fn as_struct(&self) -> Option<&HirStruct> {
        match &self.kind {
            HirItemKind::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&HirEnum> {
        match &self.kind {
            HirItemKind::Enum(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_fn_safe(&self) -> Option<&HirFn> {
        match &self.kind {
            HirItemKind::Fn(f) => Some(f),
            _ => None,
        }
    }

    pub fn generate_extra_namespace(&self) -> bool {
        match &self.kind {
            HirItemKind::Fn(_) | HirItemKind::Struct(_) | HirItemKind::Enum(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum HirItemKind {
    /// Function definition
    Fn(HirFn),
    /// Import statement
    Use(HirUse),
    /// Struct definition
    Struct(HirStruct),
    /// Type alias
    TypeAlias(HirTypeAlias),
    /// Namespace. Created from a default import.
    Namespace(HashMap<String, GlobalSymbol>),
    /// External
    External(HirExternBlock),
    /// Enum definition
    Enum(HirEnum),
}

#[derive(Clone, Debug)]
pub struct HirEnum {
    pub span: Span,
    pub ident: Ident,
    pub variants: Vec<HirEnumVariant>,
    pub generics: HirGenerics,
    pub items: HashMap<Ident, ItemId>,
}

impl HirEnum {
    pub fn get_item(&self, ident: Ident) -> Option<&ItemId> {
        self.items.get(&ident)
    }

    pub fn get_variant(&self, ident: Ident) -> Option<&HirEnumVariant> {
        self.variants.iter().find(|v| v.ident == ident)
    }
}

#[derive(Clone, Debug)]
pub struct HirEnumVariant {
    pub span: Span,
    pub ident: Ident,
    pub fields: Vec<HirEnumField>,
}

#[derive(Clone, Debug)]
pub struct HirEnumField {
    pub span: Span,
    pub ty: HirTyKind,
}

#[derive(Debug, Clone)]
pub struct HirExternBlock {
    pub abi: Option<Symbol>,
    pub items: Vec<ItemId>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirExternItemKind {
    Fn(HirFn),
    TypeAlias(HirTypeAlias),
}

#[derive(Clone, Debug)]
pub struct HirTypeAlias {
    pub span: Span,
    pub ident: Ident,
    pub ty: ComptimeValue,
    pub generics: HirGenerics,
}

#[derive(Clone, Debug)]
pub struct HirStruct {
    pub span: Span,
    pub ident: Ident,
    pub fields: Vec<HirField>,
    pub generics: HirGenerics,
    pub items: HashMap<Ident, ItemId>,
}

impl HirStruct {
    pub fn get_field(&self, name: &str) -> Option<&HirField> {
        self.fields.iter().find(|f| f.ident.to_string() == name)
    }

    pub fn get_item(&self, id: Ident) -> Option<ItemId> {
        self.items.iter().find(|(k, _)| k == &&id).map(|(_, v)| *v)
    }
}

#[derive(Clone, Debug)]
pub struct HirField {
    pub id: ItemId,
    pub span: Span,
    pub ident: Ident,
    pub ty: HirTyKind,
    pub vis: Visibility,
}

#[derive(Clone, Debug)]
pub struct HirUse {
    pub span: Span,
    pub imports: HirImportsKind,
    pub resolved_path: PathBuf,
}

#[derive(Clone, Debug)]
pub enum HirImportsKind {
    /// `use { foo, bar } from self::test;`
    List(Vec<Ident>),
    /// `use * from self::test;`
    All,
    /// `use windows from std::os::windows;`
    Default(Ident),
}

#[derive(Clone, Debug)]
pub struct HirFn {
    pub sig: HirFnSig,
    /// ID of the function body block
    pub body: Option<ItemId>,
    pub ctx: FunctionContext,
}

impl HirFn {
    /// Only if the first parameter is named `self`
    pub fn is_static(&self) -> bool {
        self.sig.params.params.first().is_none_or(|param| param.name.to_string() != "self")
    }
}

#[derive(Clone, Debug)]
pub struct HirFnSig {
    pub constant: bool,
    pub name: Ident,
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
            self.params.iter().find(|g| g.name.to_string() == *ident.to_string()).cloned()
        } else {
            None
        }
    }

    pub fn join(&self, other: &HirGenerics) -> HirGenerics {
        let mut params = self.params.clone();
        params.extend(other.params.clone());
        HirGenerics { params, span: self.span.to(other.span) }
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
            write!(f, "{}", param.ty)?;
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
        Self { span: Span::DUMMY, params: vec![] }
    }
}

#[derive(Clone, Debug)]
pub struct HirGenericArg {
    pub id: ItemId,
    pub ty: HirTyKind,
}

impl PartialEq for HirGenericArg {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}

#[derive(Clone, Debug)]
pub struct HirParam {
    pub id: ItemId,
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
    pub id: ItemId,
    pub name: Ident,
    pub kind: HirGenericKind,
}

#[derive(Clone, Debug)]
pub enum HirGenericKind {
    Type { default: Option<HirTy> },
    Const { ty: HirTy, default: Option<Lit> },
}

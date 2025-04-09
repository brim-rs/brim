use crate::{Break, Empty, ItemId, While, expr::Expr, stmts::Stmt, ty::Ty};
use brim_span::{span::Span, symbols::Symbol};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    path::PathBuf,
};

#[derive(Clone, Debug)]
pub struct Item<Kind = ItemKind> {
    pub id: ItemId,
    pub span: Span,
    pub vis: Visibility,
    pub ident: Ident,
    pub kind: Kind,
}

#[derive(Copy, Clone, Eq, Ord, PartialOrd)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.to_string().hash(state);
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name.to_string() == other.name.to_string()
    }
}

impl Ident {
    pub fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }

    pub fn is_reserved(&self) -> bool {
        self.name >= Break && self.name <= While
    }

    pub fn dummy() -> Self {
        Self { name: Empty, span: Span::initial() }
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
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
        let kind = if is_public { VisibilityKind::Public } else { VisibilityKind::Private };
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
    /// Type alias eg. `type Foo = i32;`
    TypeAlias(TypeAlias),
    /// Module declaration eg. `mod windows;`
    Module(ModuleDecl),
    /// External module declaration.
    External(ExternBlock),
    /// Namespace. Created internally by the compiler.
    Namespace(HashMap<String, (Ident, (ItemId, usize))>),
    /// Enum declaration eg. `enum Foo { ... }`
    Enum(Enum),
}

#[derive(Clone, Debug)]
pub struct Enum {
    pub span: Span,
    pub ident: Ident,
    pub variants: Vec<EnumVariant>,
    pub generics: Generics,
    pub items: Vec<Item>,
}

impl Enum {
    pub fn find(&self, ident: &Ident) -> Option<&EnumVariant> {
        self.variants.iter().find(|variant| &variant.ident == ident)
    }

    pub fn find_item(&self, ident: &Ident) -> Option<&Item> {
        self.items.iter().find(|item| &item.ident == ident)
    }
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub span: Span,
    pub ident: Ident,
    pub fields: Vec<EnumField>,
}

#[derive(Clone, Debug)]
pub struct EnumField {
    pub span: Span,
    pub ty: Ty,
}

impl ItemKind {
    pub fn as_struct(&self) -> Option<&Struct> {
        match self {
            ItemKind::Struct(struct_) => Some(struct_),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&Enum> {
        match self {
            ItemKind::Enum(enum_) => Some(enum_),
            _ => None,
        }
    }
}

impl Display for ItemKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemKind::Fn(_) => write!(f, "function"),
            ItemKind::Use(_) => write!(f, "use"),
            ItemKind::Struct(_) => write!(f, "struct"),
            ItemKind::TypeAlias(_) => write!(f, "type alias"),
            ItemKind::Module(_) => write!(f, "module"),
            ItemKind::External(_) => write!(f, "external"),
            ItemKind::Namespace(_) => write!(f, "namespace"),
            ItemKind::Enum(_) => write!(f, "enum"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternBlock {
    pub span: Span,
    pub abi: Option<Symbol>,
    pub items: Vec<Item>,
}

#[derive(Clone, Debug)]
pub struct ModuleDecl {
    pub span: Span,
    pub idents: Vec<Ident>,
}

#[derive(Clone, Debug)]
pub struct TypeAlias {
    pub span: Span,
    pub ident: Ident,
    pub ty: TypeAliasValue,
    pub generics: Generics,
}

#[derive(Clone, Debug)]
pub enum TypeAliasValue {
    Ty(Ty),
    Const(Expr),
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub span: Span,
    pub ident: Ident,
    pub fields: Vec<Field>,
    pub generics: Generics,
    pub items: Vec<Item>,
}

impl Struct {
    pub fn find_item(&self, id: Ident) -> Option<&Item> {
        self.items.iter().find(|item| item.ident == id)
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    pub id: ItemId,
    pub span: Span,
    pub ident: Ident,
    pub ty: Ty,
    pub vis: Visibility,
}

#[derive(Clone, Debug)]
pub struct Use {
    pub span: Span,
    pub path: String,
    pub imports: ImportsKind,
    pub resolved: Option<PathBuf>,
}

impl Use {
    pub fn is_dep(&self) -> bool {
        !self.path.contains('/') && !self.path.contains('\\') && !self.path.starts_with('.')
    }
}

#[derive(Clone, Debug)]
pub enum ImportsKind {
    /// `use { foo, bar } from "test";`
    List(Vec<Ident>),
    /// `use * from "test";`
    All,
    /// `use windows from std::os::windows;`
    Default(Ident),
}

#[derive(Clone, Debug)]
pub struct FnDecl {
    pub sig: FnSignature,
    pub generics: Generics,
    /// Allowed to be empty for trait functions
    pub body: Option<Block>,
    pub context: FunctionContext,
}

impl FnDecl {
    /// Isn't static if first parameter is self
    pub fn is_static(&self) -> bool {
        if self.sig.params.is_empty() {
            return true;
        }

        let first_param = &self.sig.params[0];
        !first_param.name.to_string().eq("self")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionContext {
    Item,
    Trait,
    Extern,
    Method,
}

impl FunctionContext {
    pub fn allows_self(&self) -> bool {
        match self {
            FunctionContext::Method => true,
            _ => false,
        }
    }

    pub fn allows_empty_body(&self) -> bool {
        match self {
            FunctionContext::Trait | FunctionContext::Extern => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub id: ItemId,
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
    pub id: ItemId,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub struct GenericParam {
    pub id: ItemId,
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
    pub constant: bool,
    pub name: Ident,
    pub return_type: FnReturnType,
    pub params: Vec<Param>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub ty: Ty,
    pub id: ItemId,
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

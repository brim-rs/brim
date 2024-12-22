use crate::{
    ast::statements::{Const, Function, Struct, TraitDef},
    error::span::TextSpan,
};
use std::fmt::Display;
use crate::ast::statements::Enum;

#[derive(Debug, Clone)]
pub struct UnitItem {
    pub kind: UnitItemKind,
    pub imported: bool,
    pub unit: String,
    pub public: bool,
}

#[derive(Debug, Clone)]
pub enum UnitItemKind {
    Function(Function),
    Struct(Struct),
    Trait(TraitDef),
    Const(Const),
    Enum(Enum),
}

impl Display for UnitItemKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnitItemKind::Function(_) => write!(f, "function"),
            UnitItemKind::Struct(_) => write!(f, "struct"),
            UnitItemKind::Trait(_) => write!(f, "trait"),
            UnitItemKind::Const(_) => write!(f, "const"),
            UnitItemKind::Enum(_) => write!(f, "enum"),
        }
    }
}

impl UnitItem {
    pub fn span(&self) -> TextSpan {
        match &self.kind {
            UnitItemKind::Function(f) => f.span(),
            UnitItemKind::Struct(s) => s.span(),
            UnitItemKind::Trait(t) => t.span(),
            UnitItemKind::Const(c) => c.ident.span.clone(),
            UnitItemKind::Enum(e) => e.name.span.clone(),
        }
    }
}

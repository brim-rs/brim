use brim_ast::{
    NodeId,
    item::{FnDecl, Ident},
};
use brim_index::index_type;

pub mod barrel;
pub mod modules;
pub mod walker;

index_type! {
    /// A unique identifier for a module in the compiler.
    #[derive(PartialOrd, Ord)]
    pub struct ModuleId {}
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
/// A symbol identifier.
pub struct GlobalSymbolId {
    pub mod_id: ModuleId,
    pub item_id: NodeId,
}

#[derive(Debug, Clone)]
pub struct GlobalSymbol {
    pub id: GlobalSymbolId,
    pub name: Ident,
    pub kind: GlobalSymbolKind,
    pub item_id: NodeId,
}

impl GlobalSymbol {
    pub fn new(name: Ident, kind: GlobalSymbolKind, id: NodeId, gid: GlobalSymbolId) -> Self {
        Self {
            name,
            kind,
            item_id: id,
            id: gid,
        }
    }
}

#[derive(Debug, Clone)]
pub enum GlobalSymbolKind {
    Fn(FnDecl),
}

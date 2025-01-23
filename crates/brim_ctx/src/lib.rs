use brim_ast::item::{FnDecl, Ident};
use brim_ast::NodeId;
use brim_index::index_type;

pub mod barrel;
pub mod compiler;
pub mod diag_ctx;
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
    pub name: Ident,
    pub kind: GlobalSymbolKind,
    pub item_id: NodeId,
}

impl GlobalSymbol {
    pub fn new(name: Ident, kind: GlobalSymbolKind, id:NodeId) -> Self {
        Self { name, kind, item_id: id }
    }
}

#[derive(Debug, Clone)]
pub enum GlobalSymbolKind {
    Fn(FnDecl)
}
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

#[derive(Debug, Clone)]
/// A symbol identifier. 
pub struct SymbolId {
    pub mod_id: ModuleId,
    pub item_id: NodeId
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: Ident,
    pub kind: SymbolKind,
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Fn(FnDecl)
}
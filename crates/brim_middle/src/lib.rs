use brim_ast::{
    ItemId,
    item::{FnDecl, Ident, Struct, Visibility},
    ty::Ty,
};
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_index::index_type;
use brim_span::span::Span;
use std::{collections::HashMap, fmt::Debug};
use tracing::debug;

pub mod args;
pub mod barrel;
pub mod builtins;
pub mod experimental;
pub mod lints;
pub mod modules;
pub mod temp_diag;
pub mod walker;

index_type! {
    #[derive(PartialOrd, Ord)]
    pub struct ModuleId {}
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Location {
    pub mod_id: ModuleId,
    pub item_id: ItemId,
}

impl Location {
    pub fn new(mod_id: ModuleId, item_id: ItemId) -> Self {
        Self { mod_id, item_id }
    }
}

#[derive(Clone, Debug)]
pub struct GlobalSymbol {
    pub id: Location,
    pub name: Ident,
}

impl GlobalSymbol {
    pub fn new(name: Ident, location: Location) -> Self {
        Self { name, id: location }
    }
}

#[derive(Diagnostic)]
#[error("experimental feature `{feature}` is not enabled on this build")]
pub struct ExperimentalFeatureNotEnabled {
    #[error]
    pub span: (Span, usize),
    pub feature: String,
    #[note]
    pub note: String,
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub symbols: HashMap<usize, Vec<GlobalSymbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, file_id: usize, symbol: GlobalSymbol) {
        debug!("Adding symbol: {}", symbol.name);

        self.symbols
            .entry(file_id)
            .or_insert_with(Vec::new)
            .push(symbol);
    }

    pub fn get_by_ident(&self, ident: &Ident, file_id: usize) -> Option<&GlobalSymbol> {
        self.symbols.get(&file_id).and_then(|symbols| {
            symbols
                .iter()
                .find(|symbol| symbol.name.to_string() == *ident.to_string())
        })
    }

    pub fn by_module(&self, mod_id: ModuleId) -> Vec<GlobalSymbol> {
        self.symbols
            .values()
            .flatten()
            .filter(|symbol| symbol.id.mod_id == mod_id)
            .cloned()
            .collect()
    }

    pub fn create_map(&self, mod_id: ModuleId) -> HashMap<String, GlobalSymbol> {
        self.by_module(mod_id)
            .into_iter()
            .map(|symbol| (symbol.name.clone().to_string(), symbol))
            .collect()
    }

    pub fn resolve(&self, ident: &String, mod_id: usize) -> Option<GlobalSymbol> {
        self.symbols
            .get(&mod_id)
            .and_then(|symbols| {
                symbols
                    .iter()
                    .find(|symbol| symbol.name.to_string() == *ident)
            })
            .cloned()
    }

    pub fn iter_symbols(&self) -> impl Iterator<Item = &GlobalSymbol> {
        self.symbols.values().flatten()
    }
}

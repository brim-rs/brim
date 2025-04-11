use brim_ast::{
    ItemId,
    item::{Ident, Item},
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
    pub struct ModuleId {}
}

#[derive(Debug, Clone, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct Location {
    pub mod_id: ModuleId,
    pub item_id: ItemId,
}

impl Location {
    pub fn new(mod_id: ModuleId, item_id: ItemId) -> Self {
        Self { mod_id, item_id }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct GlobalSymbol {
    pub id: Location,
    pub name: Ident,
}

impl GlobalSymbol {
    pub fn new(name: Ident, location: Location) -> Self {
        Self { name, id: location }
    }

    pub fn from_temp(val: (Ident, (ItemId, usize))) -> Self {
        let item_id = val.1.0;
        let mod_id = val.1.1;
        Self::new(val.0, Location::new(ModuleId::from_usize(mod_id), item_id))
    }

    pub fn into_temp(self) -> (Ident, (ItemId, usize)) {
        let item_id = self.id.item_id;
        let mod_id = self.id.mod_id.as_usize();
        (self.name, (item_id, mod_id))
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

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { symbols: HashMap::new() }
    }

    pub fn add_symbol(&mut self, file_id: usize, symbol: GlobalSymbol) {
        debug!("Adding symbol: {}", symbol.name);

        self.symbols.entry(file_id).or_default().push(symbol);
    }

    pub fn get_by_ident(&self, ident: &Ident, file_id: usize) -> Option<&GlobalSymbol> {
        self.symbols.get(&file_id).and_then(|symbols| {
            symbols.iter().find(|symbol| symbol.name.to_string() == *ident.to_string())
        })
    }
    
    pub fn get_by_id(&self, id: &ItemId) -> Option<&GlobalSymbol> {
        self.symbols
            .values()
            .flatten()
            .find(|symbol| symbol.id.item_id == *id)
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
            .and_then(|symbols| symbols.iter().find(|symbol| symbol.name.to_string() == *ident))
            .cloned()
    }

    pub fn iter_symbols(&self) -> impl Iterator<Item = &GlobalSymbol> {
        self.symbols.values().flatten()
    }
}

#[derive(Debug, Clone)]
pub struct SimpleModules {
    pub items: HashMap<ItemId, Item>,
}

impl SimpleModules {
    pub fn get_item(&self, item_id: ItemId) -> &Item {
        self.items.get(&item_id).unwrap_or_else(|| {
            panic!("tried to query an item with id: {item_id:?}, but it doesn't exist")
        })
    }
}

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

#[derive(Clone)]
pub struct GlobalSymbol<Kind = GlobalSymbolKind> {
    pub id: Location,
    pub name: Ident,
    pub kind: Kind,
    pub vis: Visibility,
}

impl<Kind: Clone + Debug> Debug for GlobalSymbol<Kind> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (mod {:?})", self.name, self.id.mod_id)
    }
}

impl<Kind> GlobalSymbol<Kind> {
    pub fn new(name: Ident, kind: Kind, location: Location, vis: Visibility) -> Self {
        Self {
            name,
            kind,
            id: location,
            vis,
        }
    }
}

#[derive(Debug, Clone)]
pub enum GlobalSymbolKind {
    Fn(FnDecl),
    Struct(Struct),
    Namespace(HashMap<Ident, GlobalSymbol>),
    Ty(Ty),
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

#[derive(Clone)]
pub struct SymbolTable<Kind = GlobalSymbolKind>
where
    Kind: Clone + Debug,
{
    pub symbols: HashMap<usize, Vec<GlobalSymbol<Kind>>>,
}

impl<Kind: Clone + Debug> Default for SymbolTable<Kind>
where
    HashMap<Ident, GlobalSymbol<Kind>>: FromIterator<(Ident, GlobalSymbol<Kind>)>,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<Kind: Clone + Debug> SymbolTable<Kind>
where
    HashMap<Ident, GlobalSymbol<Kind>>: FromIterator<(Ident, GlobalSymbol<Kind>)>,
{
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, file_id: usize, symbol: GlobalSymbol<Kind>) {
        debug!("Adding symbol: {}", symbol.name);

        self.symbols
            .entry(file_id)
            .or_insert_with(Vec::new)
            .push(symbol);
    }

    pub fn get_by_ident(&self, ident: &Ident, file_id: usize) -> Option<&GlobalSymbol<Kind>> {
        self.symbols.get(&file_id).and_then(|symbols| {
            symbols
                .iter()
                .find(|symbol| symbol.name.to_string() == *ident.to_string())
        })
    }

    pub fn by_module(&self, mod_id: ModuleId) -> Vec<GlobalSymbol<Kind>> {
        self.symbols
            .values()
            .flatten()
            .filter(|symbol| symbol.id.mod_id == mod_id)
            .cloned()
            .collect()
    }

    pub fn create_map(&self, mod_id: ModuleId) -> HashMap<Ident, GlobalSymbol<Kind>> {
        self.by_module(mod_id)
            .into_iter()
            .map(|symbol| (symbol.name.clone(), symbol))
            .collect()
    }

    pub fn resolve(&self, ident: &String, mod_id: usize) -> Option<GlobalSymbol<Kind>> {
        self.symbols
            .get(&mod_id)
            .and_then(|symbols| {
                symbols
                    .iter()
                    .find(|symbol| symbol.name.to_string() == *ident)
            })
            .cloned()
    }

    pub fn iter_symbols(&self) -> impl Iterator<Item = &GlobalSymbol<Kind>> {
        self.symbols.values().flatten()
    }
}

impl<Kind: Clone + Debug> Debug for SymbolTable<Kind> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.symbols.iter()).finish()
    }
}

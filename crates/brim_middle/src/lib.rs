use brim_ast::{
    NodeId,
    item::{FnDecl, Ident, Struct, Visibility},
};
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_index::index_type;
use brim_span::span::Span;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};
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
    /// A unique identifier for a module in the compiler.
    #[derive(PartialOrd, Ord)]
    pub struct ModuleId {}
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
/// A symbol identifier.
pub struct Location {
    pub mod_id: ModuleId,
    pub item_id: NodeId,
}

#[derive(Clone)]
pub struct GlobalSymbol {
    pub id: Location,
    pub name: Ident,
    pub kind: GlobalSymbolKind,
    pub item_id: NodeId,
    pub vis: Visibility,
}

impl GlobalSymbol {
    pub fn new(
        name: Ident,
        kind: GlobalSymbolKind,
        id: NodeId,
        gid: Location,
        vis: Visibility,
    ) -> Self {
        Self {
            name,
            kind,
            item_id: id,
            id: gid,
            vis,
        }
    }

    pub fn span(&self) -> Span {
        match self.kind {
            GlobalSymbolKind::Fn(ref decl) => decl.sig.span,
            GlobalSymbolKind::Struct(ref decl) => decl.span,
            GlobalSymbolKind::Namespace(_) => todo!(),
        }
    }

    pub fn with_mod_id(&mut self, mod_id: ModuleId) -> Self {
        self.id.mod_id = mod_id;
        self.clone()
    }
}

#[cfg(debug_assertions)]
impl Debug for GlobalSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (mod {:?})", self.name, self.id.mod_id)
    }
}

#[derive(Debug, Clone)]
pub enum GlobalSymbolKind {
    Fn(FnDecl),
    Struct(Struct),
    Namespace(HashMap<Ident, GlobalSymbol>),
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
pub struct SymbolTable {
    pub symbols: HashMap<usize, Vec<GlobalSymbol>>,
}

impl SymbolTable {
    /// Creates a new empty SymbolTable.
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    /// Adds a global symbol to the table for a specific file.
    pub fn add_symbol(&mut self, file_id: usize, symbol: GlobalSymbol) {
        debug!("Adding symbol: {}", symbol.name);

        self.symbols
            .entry(file_id)
            .or_insert_with(Vec::new)
            .push(symbol);
    }

    /// Finds a symbol by its identifier in a specific file.
    pub fn get_by_ident(&self, ident: &Ident, file_id: usize) -> Option<&GlobalSymbol> {
        self.symbols.get(&file_id).and_then(|symbols| {
            symbols
                .iter()
                .find(|symbol| symbol.name.to_string() == *ident.to_string())
        })
    }

    /// Returns all symbols belonging to a specific module.
    pub fn by_module(&self, mod_id: ModuleId) -> Vec<GlobalSymbol> {
        self.symbols
            .values()
            .flatten()
            .filter(|symbol| symbol.id.mod_id == mod_id)
            .cloned()
            .collect()
    }

    /// Creates a mapping from identifiers to symbols for a specific module.
    pub fn create_map(&self, mod_id: ModuleId) -> HashMap<Ident, GlobalSymbol> {
        self.by_module(mod_id)
            .into_iter()
            .map(|symbol| (symbol.name.clone(), symbol))
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
}

#[cfg(debug_assertions)]
impl Debug for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.symbols.iter()).finish()
    }
}

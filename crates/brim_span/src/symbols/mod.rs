use brim_index::index_type;
use once_cell::sync::Lazy;
use std::{collections::HashMap, fmt::Display, sync::Mutex};

index_type! {
    /// A unique identifier for a symbol interned in the global interner.
    #[derive(PartialOrd, Ord)]
    pub struct SymbolIndex {}
}

/// A symbol representing an interned string.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Symbol(pub SymbolIndex);

impl Symbol {
    /// Creates a symbol from a string by interning it.
    #[inline]
    pub fn new(s: &str) -> Self {
        Self(SymbolIndex::from_usize(intern(s)))
    }

    /// Resolves this symbol back to its original string.
    #[inline]
    pub fn as_str(&self) -> Option<String> {
        resolve(self.0.as_usize())
    }
}

#[derive(Debug)]
pub struct SymbolInterner {
    strings: HashMap<String, SymbolIndex>,
    symbols: Vec<String>,
    pub initialized: bool,
}

impl SymbolInterner {
    pub fn new() -> Self {
        Self {
            strings: HashMap::new(),
            symbols: Vec::new(),
            initialized: false,
        }
    }

    pub fn add_existing(&mut self, index: SymbolIndex, value: String) {
        self.strings.insert(value.clone(), index);
        self.symbols.push(value);
    }

    pub fn intern(&mut self, s: &str) -> SymbolIndex {
        if let Some(&index) = self.strings.get(s) {
            index
        } else {
            let index = SymbolIndex::from_usize(self.symbols.len());
            self.symbols.push(s.to_string());
            self.strings.insert(s.to_string(), index);
            index
        }
    }

    pub fn resolve(&self, symbol: SymbolIndex) -> Option<&str> {
        self.symbols.get(symbol.as_usize()).map(String::as_str)
    }

    /// Returns the number of interned symbols
    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    /// Returns true if no symbols have been interned
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }
}

pub static GLOBAL_INTERNER: Lazy<Mutex<SymbolInterner>> =
    Lazy::new(|| Mutex::new(SymbolInterner::new()));

/// Interns a string in the global interner and returns its index.
#[inline]
fn intern(s: &str) -> usize {
    GLOBAL_INTERNER
        .lock()
        .expect("Failed to lock global interner")
        .intern(s)
        .as_usize()
}

/// Resolves a symbol index to its original string.
#[inline]
fn resolve(index: usize) -> Option<String> {
    GLOBAL_INTERNER
        .lock()
        .expect("Failed to lock global interner")
        .resolve(SymbolIndex::from_usize(index))
        .map(ToOwned::to_owned)
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.as_str() {
            write!(f, "{}", s)
        } else {
            panic!("Failed to resolve symbol")
        }
    }
}

impl From<String> for Symbol {
    fn from(s: String) -> Self {
        Symbol::new(&s)
    }
}

impl From<&str> for Symbol {
    fn from(s: &str) -> Self {
        Symbol::new(s)
    }
}

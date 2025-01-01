use once_cell::sync::Lazy;
use std::sync::Mutex;
use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolInterner {
    strings: HashMap<String, usize>,
    indices: Vec<String>,
}

impl SymbolInterner {
    pub fn new() -> Self {
        Self {
            strings: HashMap::new(),
            indices: Vec::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> usize {
        if let Some(&index) = self.strings.get(s) {
            index
        } else {
            let index = self.indices.len();
            self.indices.push(s.to_string());
            self.strings.insert(s.to_string(), index);
            index
        }
    }

    pub fn resolve(&self, symbol: usize) -> Option<&str> {
        self.indices.get(symbol).map(String::as_str)
    }
}

pub static GLOBAL_INTERNER: Lazy<Mutex<SymbolInterner>> = Lazy::new(|| Mutex::new(SymbolInterner::new()));

pub fn intern(s: &str) -> usize {
    GLOBAL_INTERNER.lock().unwrap().intern(s)
}

pub fn resolve(symbol: usize) -> Option<String> {
    GLOBAL_INTERNER.lock().unwrap().resolve(symbol).map(|s| s.to_string())
}
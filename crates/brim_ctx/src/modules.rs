use std::collections::HashMap;
use crate::{barrel::Barrel, compiler::CompilerContext, walker::AstWalker, GlobalSymbol, GlobalSymbolId, GlobalSymbolKind, ModuleId};
use std::path::PathBuf;
use brim_ast::item::{ImportsKind, Item, ItemKind};
use brim_ast::NodeId;

#[derive(Debug, Clone)]
pub struct Module {
    pub path: PathBuf,
    pub barrel: Barrel,
    pub imports: Vec<GlobalSymbolId>,
}

#[derive(Debug, Clone)]
pub struct ModuleMap {
    pub modules: Vec<Module>,
    pub symbols: HashMap<GlobalSymbolId, GlobalSymbol>,
    pub imports: HashMap<GlobalSymbolId, PathBuf>,
}

impl ModuleMap {
    pub fn insert_or_update(&mut self, path: PathBuf, barrel: Barrel) {
        for module in &mut self.modules {
            if module.path == path {
                module.barrel = barrel;
                return;
            }
        }

        self.modules.push(Module { path, barrel, imports: vec![] });
    }

    pub fn add_import(&mut self, symbol_id: GlobalSymbolId, path: PathBuf) {
        self.imports.insert(symbol_id, path);
    }

    pub fn module_by_import(&self, symbol_id: GlobalSymbolId) -> Option<&Module> {
        self.imports.get(&symbol_id).and_then(|path| {
            self.modules.iter().find(|module| module.path == *path)
        })
    }

    pub fn update_modules_imports(&mut self, mod_id: ModuleId, imports: Vec<GlobalSymbolId>) {
        for module in &mut self.modules {
            if ModuleId::from_usize(module.barrel.file_id) == mod_id {
                module.imports = imports;
                return;
            }
        }
    }

    pub fn add_symbol(&mut self, symbol: GlobalSymbol, file: usize, item_id: NodeId) {
        self.symbols.insert(GlobalSymbolId {
            mod_id: ModuleId::from_usize(file),
            item_id,
        }, symbol);
    }

    pub fn find_symbol_by_name(&self, name: &str, module_id: Option<ModuleId>) -> Vec<&GlobalSymbol> {
        self.symbols.iter()
            .filter(|(id, symbol)| {
                symbol.name.to_string() == name &&
                    module_id.map_or(true, |mid| id.mod_id == mid)
            })
            .map(|(_, symbol)| symbol)
            .collect()
    }

    pub fn find_symbols_in_module(&self, module_id: Option<ModuleId>) -> Vec<&GlobalSymbol> {
        self.symbols.iter()
            .filter(|(id, _)| module_id.map_or(true, |mid| id.mod_id == mid))
            .map(|(_, symbol)| symbol)
            .collect()
    }

    pub fn has_symbol(&self, name: &str, module_id: Option<ModuleId>) -> bool {
        self.symbols.iter().any(|(id, symbol)|
            symbol.name.to_string() == name &&
                module_id.map_or(true, |mid| id.mod_id == mid)
        )
    }

    pub fn get_function_symbols(&self, module_id: Option<ModuleId>) -> Vec<&GlobalSymbol> {
        self.symbols.iter()
            .filter(|(id, symbol)| {
                matches!(symbol.kind, GlobalSymbolKind::Fn(_)) &&
                    module_id.map_or(true, |mid| id.mod_id == mid)
            })
            .map(|(_, symbol)| symbol)
            .collect()
    }

    pub fn find_symbol_by<F>(&self, predicate: F, module_id: Option<ModuleId>) -> Vec<&GlobalSymbol>
    where
        F: Fn(&GlobalSymbol) -> bool,
    {
        self.symbols.iter()
            .filter(|(id, symbol)|
                predicate(symbol) &&
                    module_id.map_or(true, |mid| id.mod_id == mid)
            )
            .map(|(_, symbol)| symbol)
            .collect()
    }
}

#[derive(Debug)]
pub struct SymbolCollector<'a> {
    pub map: &'a mut ModuleMap,
    pub file_id: usize,
}

impl<'a> SymbolCollector<'a> {
    pub fn new(map: &'a mut ModuleMap) -> Self {
        Self { map, file_id: 0 }
    }

    pub fn collect(&mut self) {
        let mut modules = std::mem::take(&mut self.map.modules);

        for module in &mut modules {
            self.file_id = module.barrel.file_id;

            for item in module.barrel.items.iter_mut() {
                self.visit_item(item);
            }
        }

        self.map.modules = modules;
    }
}

impl<'a> AstWalker for SymbolCollector<'a> {
    fn visit_item(&mut self, item: &mut Item) {
        match &item.kind {
            ItemKind::Fn(f) => {
                self.map.add_symbol(
                    GlobalSymbol::new(item.ident, GlobalSymbolKind::Fn(f.clone()), item.id),
                    self.file_id,
                    item.id,
                );
            }
            _ => {}
        }
    }
}
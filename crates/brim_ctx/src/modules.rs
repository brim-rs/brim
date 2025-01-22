use std::collections::HashMap;
use crate::{barrel::Barrel, compiler::CompilerContext, walker::AstWalker, GlobalSymbol, GlobalSymbolId, GlobalSymbolKind, ModuleId};
use std::path::PathBuf;
use brim_ast::item::{Item, ItemKind};
use brim_ast::NodeId;

#[derive(Debug, Clone)]
pub struct Module {
    pub path: PathBuf,
    pub barrel: Barrel,
}

#[derive(Debug, Clone)]
pub struct ModuleMap {
    pub modules: Vec<Module>,
    pub symbols: HashMap<GlobalSymbolId, GlobalSymbol>,
}

impl ModuleMap {
    pub fn insert_or_update(&mut self, path: PathBuf, barrel: Barrel) {
        for module in &mut self.modules {
            if module.path == path {
                module.barrel = barrel;
                return;
            }
        }

        self.modules.push(Module { path, barrel });
    }

    pub fn add_symbol(&mut self, symbol: GlobalSymbol, file: usize, item_id: NodeId) {
        self.symbols.insert(GlobalSymbolId {
            mod_id: ModuleId::from_usize(file),
            item_id,
        }, symbol);
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
            let mut items = std::mem::take(&mut module.barrel.items);

            for item in &mut items {
                self.visit_item(item);
            }

            module.barrel.items = items;
        }

        self.map.modules = modules;
    }
}

impl<'a> AstWalker for SymbolCollector<'a> {
    fn visit_item(&mut self, item: &mut Item) {
        match &item.kind {
            ItemKind::Fn(f) => {
                self.map.add_symbol(GlobalSymbol::new(item.ident, GlobalSymbolKind::Fn(f.clone())), self.file_id, item.id);
            }
            _ => {}
        }
    }
}
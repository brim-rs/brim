use std::collections::HashMap;
use crate::{barrel::Barrel, compiler::CompilerContext, walker::AstWalker, Symbol, SymbolId};
use std::path::PathBuf;
use brim_ast::item::Item;

#[derive(Debug, Clone)]
pub struct Module {
    pub path: PathBuf,
    pub barrel: Barrel,
}

#[derive(Debug, Clone)]
pub struct ModuleMap {
    pub modules: Vec<Module>,
    pub symbols: HashMap<SymbolId, Symbol>,
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
}

#[derive(Debug)]
pub struct SymbolCollector<'a> {
    pub map: &'a mut ModuleMap,
}

impl<'a> SymbolCollector<'a> {
    pub fn new(map: &'a mut ModuleMap) -> Self {
        Self { map }
    }

    pub fn collect(&mut self) {
        let mut modules = std::mem::take(&mut self.map.modules);

        for module in &mut modules {
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
    fn visit_item(&mut self, item: &mut Item) {}
}
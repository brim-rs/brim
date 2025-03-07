use crate::{GlobalSymbol, Location, ModuleId, SymbolTable, barrel::Barrel, walker::AstWalker};
use brim_ast::{
    ItemId,
    item::{Ident, ImportsKind, Item, ItemKind},
};
use brim_span::files::get_id_by_name;
use std::{collections::HashMap, path::PathBuf};

#[derive(Debug, Clone)]
pub struct Module {
    pub path: PathBuf,
    pub barrel: Barrel,
    pub imports: Vec<Location>,
}

#[derive(Debug, Clone)]
pub struct ModuleMap {
    pub modules: Vec<Module>,
}

impl ModuleMap {
    pub fn new() -> Self {
        Self { modules: vec![] }
    }

    pub fn insert_or_update(&mut self, path: PathBuf, barrel: Barrel) {
        for module in &mut self.modules {
            if module.path == path {
                module.barrel = barrel;
                return;
            }
        }

        self.modules.push(Module {
            path,
            barrel,
            imports: vec![],
        });
    }

    pub fn update_modules_imports(&mut self, mod_id: ModuleId, imports: Vec<Location>) {
        for module in &mut self.modules {
            if ModuleId::from_usize(module.barrel.file_id) == mod_id {
                module.imports = imports;
                return;
            }
        }
    }

    pub fn get_module_by_id(&self, id: ModuleId) -> Option<&Module> {
        self.modules
            .iter()
            .find(|module| ModuleId::from_usize(module.barrel.file_id) == id)
    }
}

#[derive(Debug)]
pub struct SymbolCollector<'a> {
    pub table: &'a mut SymbolTable,
    pub file_id: usize,
}

impl<'a> SymbolCollector<'a> {
    pub fn new(table: &'a mut SymbolTable) -> Self {
        Self { table, file_id: 0 }
    }

    pub fn collect(&mut self, map: &mut ModuleMap) {
        for module in map.modules.iter_mut() {
            self.file_id = module.barrel.file_id;

            for item in module.barrel.items.iter_mut() {
                self.visit_item(item);
            }
        }
    }
}

impl<'a> AstWalker for SymbolCollector<'a> {
    fn visit_item(&mut self, item: &mut Item) {
        let id = Location {
            mod_id: ModuleId::from_usize(self.file_id),
            item_id: item.id,
        };

        match &item.kind {
            ItemKind::Fn(f) => {
                self.table
                    .add_symbol(self.file_id, GlobalSymbol::new(item.ident, id));
            }
            ItemKind::Struct(s) => {
                self.table
                    .add_symbol(self.file_id, GlobalSymbol::new(item.ident, id));
            }
            ItemKind::TypeAlias(t) => {
                self.table
                    .add_symbol(self.file_id, GlobalSymbol::new(item.ident, id));
            }
            _ => {}
        }
    }
}

#[derive(Debug)]
/// This has to run after the SymbolCollector, because it needs the symbols to be in the table.
pub struct UseCollector<'a> {
    pub table: &'a mut SymbolTable,
    pub file_id: usize,
    pub namespaces: HashMap<(Ident, ItemId), HashMap<Ident, GlobalSymbol>>,
}

impl<'a> UseCollector<'a> {
    pub fn new(table: &'a mut SymbolTable) -> Self {
        Self {
            table,
            file_id: 0,
            namespaces: HashMap::new(),
        }
    }

    pub fn collect(&mut self, map: &mut ModuleMap) {
        for module in map.modules.iter_mut() {
            self.file_id = module.barrel.file_id;

            for item in module.barrel.items.iter_mut() {
                self.visit_item(item);
            }
        }
    }
}

impl<'a> AstWalker for UseCollector<'a> {
    fn visit_item(&mut self, item: &mut Item) {
        match &item.kind {
            ItemKind::Use(use_stmt) => {
                let id = get_id_by_name(&use_stmt.resolved.clone().unwrap()).unwrap();

                let mut symbols: Vec<GlobalSymbol> = vec![];

                let mod_id = ModuleId::from_usize(id);
                match &use_stmt.imports {
                    ImportsKind::All => {
                        symbols = self.table.by_module(mod_id);
                    }
                    ImportsKind::List(list) => {
                        for import in list {
                            let symbol = self.table.get_by_ident(import, id).unwrap();

                            symbols.push(symbol.clone());
                        }
                    }
                    // use windows from std::os::windows;
                    ImportsKind::Default(ident) => {
                        let map = self.table.create_map(mod_id);

                        let id = ItemId::new();
                        self.namespaces.insert((ident.clone(), id), map);
                        symbols.push(GlobalSymbol::new(ident.clone(), Location {
                            mod_id,
                            item_id: id,
                        }))
                    }
                }

                for symbol in symbols {
                    self.table.add_symbol(self.file_id, symbol.clone());
                }
            }
            _ => {}
        }
    }
}

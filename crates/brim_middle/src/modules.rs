use crate::{
    GlobalSymbol, Location, ModuleId, SimpleModules, SymbolTable, barrel::Barrel,
    temp_diag::TemporaryDiagnosticContext, walker::AstWalker,
};
use brim_ast::{
    ItemId,
    item::{Ident, ImportsKind, Item, ItemKind},
};
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::{files::get_id_by_name, span::Span};
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
    pub simple: &'a mut SimpleModules,
}

impl<'a> SymbolCollector<'a> {
    pub fn new(table: &'a mut SymbolTable, simple: &'a mut SimpleModules) -> Self {
        Self {
            table,
            file_id: 0,
            simple,
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

impl<'a> AstWalker for SymbolCollector<'a> {
    fn visit_item(&mut self, item: &mut Item) {
        let id = Location {
            mod_id: ModuleId::from_usize(self.file_id),
            item_id: item.id,
        };

        self.simple.items.insert(item.id, item.clone());
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
            ItemKind::External(external) => {
                for item in external.items.clone() {
                    self.table.add_symbol(
                        self.file_id,
                        GlobalSymbol::new(item.ident.clone(), Location {
                            mod_id: ModuleId::from_usize(self.file_id),
                            item_id: item.id,
                        }),
                    );
                }
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
    pub namespaces: HashMap<(Ident, ItemId), HashMap<String, GlobalSymbol>>,
    pub ctx: TemporaryDiagnosticContext,
}

impl<'a> UseCollector<'a> {
    pub fn new(table: &'a mut SymbolTable) -> Self {
        Self {
            table,
            file_id: 0,
            namespaces: HashMap::new(),
            ctx: TemporaryDiagnosticContext::new(),
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
                let id = get_id_by_name(&use_stmt.resolved.clone().unwrap()).expect(&format!(
                    "Failed to get id for module: {}",
                    use_stmt.resolved.clone().unwrap().display()
                ));

                let mut symbols: Vec<GlobalSymbol> = vec![];

                let mod_id = ModuleId::from_usize(id);
                match &use_stmt.imports {
                    ImportsKind::All => {
                        symbols = self.table.by_module(mod_id);
                    }
                    ImportsKind::List(list) => {
                        for import in list {
                            let symbol = self.table.get_by_ident(import, id);

                            if let Some(symbol) = symbol {
                                symbols.push(symbol.clone());
                            } else {
                                self.ctx.emit(Box::new(UseError {
                                    span: (import.span.clone(), self.file_id),
                                    symbol: import.to_string(),
                                }));
                            }
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

#[derive(Diagnostic)]
#[error("symbol `{symbol}` not found")]
pub struct UseError {
    #[error]
    pub span: (Span, usize),
    pub symbol: String,
}

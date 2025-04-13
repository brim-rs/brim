#![feature(let_chains)]

use crate::{
    expr::HirExpr,
    items::HirItem,
    stmts::HirStmt,
    transformer::{HirModule, HirModuleMap},
    ty::HirTyKind,
};
use brim_ast::ItemId;
use brim_config::toml::Config;
use brim_fs::paths_equal;
use brim_middle::{GlobalSymbol, SymbolTable};
use std::{collections::HashMap, path::PathBuf};

pub mod builtin;
pub mod comptime;
mod errors;
pub mod expr;
pub mod inference;
pub mod items;
pub mod stmts;
pub mod transformer;
pub mod ty;
pub mod type_checker;

pub trait Codegen {
    fn generate(&mut self, compiled: &CompiledModules);

    fn generate_module(&mut self, module: HirModule, compiled: &CompiledModules);

    fn generate_item(&mut self, item: HirItem, compiled: &CompiledModules);

    fn generate_expr(&mut self, expr: HirExpr) -> String;

    fn generate_stmt(&mut self, stmt: HirStmt) -> String;

    fn generate_ty(&mut self, ty: HirTyKind) -> String;
}

#[derive(Clone, Debug)]
pub struct CompiledModule {
    pub config: Config,
    pub hir: HirModuleMap,
}

#[derive(Debug, Clone)]
pub struct CompiledModules {
    pub map: HashMap<String, CompiledModule>,
    pub symbols: SymbolTable,
    pub items: HashMap<ItemId, HirItem>,
    pub assigned_paths: HashMap<ItemId, ItemId>,
    // tracks which items use which other items
    pub item_relations: HashMap<GlobalSymbol, Vec<GlobalSymbol>>,
    pub expanded_by_builtins: HashMap<ItemId, String>,
    pub builtin_args: HashMap<ItemId, Vec<HirExpr>>,
    //          enum variant -> enum id
    pub enums: HashMap<ItemId, ItemId>,
}

impl Default for CompiledModules {
    fn default() -> Self {
        Self::new()
    }
}

impl CompiledModules {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            symbols: SymbolTable::new(),
            items: HashMap::new(),
            assigned_paths: HashMap::new(),
            item_relations: HashMap::new(),
            expanded_by_builtins: HashMap::new(),
            builtin_args: HashMap::new(),
            enums: HashMap::new(),
        }
    }

    pub fn insert_item(&mut self, item: HirItem) {
        self.items.insert(item.id, item);
    }

    pub fn resolve_symbol(&self, ident: &String, module: usize) -> Option<&HirItem> {
        if let Some(sym) = self.symbols.resolve(ident, module) {
            self.items.get(&sym.id.item_id)
        } else {
            None
        }
    }

    pub fn get_item(&self, item_id: ItemId) -> &HirItem {
        self.items.get(&item_id).unwrap_or_else(|| {
            panic!("tried to query an item with id: {item_id:?}, but it doesn't exist")
        })
    }

    pub fn assign_path(&mut self, item_id: ItemId, path: ItemId) {
        self.assigned_paths.insert(item_id, path);
    }

    pub fn get_assigned_path(&self, item_id: ItemId) -> ItemId {
        *self
            .assigned_paths
            .get(&item_id)
            .unwrap_or_else(|| panic!("an assigned path with id: {item_id:?} doesn't exist"))
    }

    pub fn add_item_relation(&mut self, item_id: GlobalSymbol, relation: GlobalSymbol) {
        self.item_relations.entry(item_id).or_default().push(relation);
    }

    pub fn get_item_relations(&self, item_id: GlobalSymbol) -> Option<&Vec<GlobalSymbol>> {
        self.item_relations.get(&item_id)
    }

    pub fn add_enum(&mut self, variant_id: ItemId, item_id: ItemId) {
        self.enums.insert(variant_id, item_id);
    }

    pub fn get_enum_by_variant(&self, variant_id: ItemId) -> &ItemId {
        self.enums.get(&variant_id).unwrap_or_else(|| {
            panic!("tried to query an enum with variant id: {variant_id:?}, but it doesn't exist")
        })
    }

    /// Finds module by path in the whole structure.
    pub fn find_module_by_path(&self, path: PathBuf) -> Option<&HirModule> {
        self.map.iter().find_map(|(_, module)| {
            module.hir.modules.iter().find(|module| paths_equal(&module.path, &path))
        })
    }
}

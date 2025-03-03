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
use brim_middle::SymbolTable;
use std::collections::HashMap;

pub mod builtin;
pub mod comptime;
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
}

impl CompiledModules {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            symbols: SymbolTable::new(),
            items: HashMap::new(),
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
        self.items.get(&item_id).unwrap()
    }
}

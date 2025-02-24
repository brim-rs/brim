#![feature(let_chains)]

use brim_hir::transformer::HirModuleMap;
use brim_middle::SymbolTable;
use std::collections::HashMap;
use brim_config::toml::Config;

pub mod compiler;
pub mod diag_ctx;
pub mod errors;
pub mod name;
pub mod validator;

#[derive(Clone, Debug)]
pub struct CompiledModule {
    pub config: Config,
    pub hir: HirModuleMap,
}

#[derive(Debug, Clone)]
pub struct CompiledModules {
    pub map: HashMap<String, CompiledModule>,
    pub symbols: SymbolTable,
}

impl CompiledModules {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            symbols: SymbolTable::new(),
        }
    }
}

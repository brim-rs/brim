extern crate core;

pub mod discover;
pub mod resolver;
pub mod session;

pub use brim_ast::*;
use brim_config::toml::Config;
pub use brim_config::*;
pub use brim_ctx::*;
pub use brim_diag_macro::*;
pub use brim_diagnostics::*;
pub use brim_fs::*;
use brim_hir::transformer::HirModuleMap;
pub use brim_hir::*;
pub use brim_lexer::*;
pub use brim_middle::*;
pub use brim_shell::*;
pub use brim_span::*;
use std::collections::HashMap;

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

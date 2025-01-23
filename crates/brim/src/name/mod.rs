mod scopes;
mod errors;

use tracing::debug;
use brim_ast::item::{Block, FnDecl, Item};
use brim_ast::stmts::Let;
use brim_ctx::compiler::CompilerContext;
use brim_ctx::modules::ModuleMap;
use brim_ctx::walker::AstWalker;
use brim_diagnostics::diag_opt;
use brim_fs::loader::BrimFileLoader;
use crate::name::scopes::{ScopeManager, VariableInfo};

#[derive(Debug)]
pub struct NameResolver<'a> {
    pub ctx: &'a mut CompilerContext<'a>,
    pub map: ModuleMap,
    pub scopes: ScopeManager,
    pub file: usize,
}

impl<'a> NameResolver<'a> {
    pub fn new(ctx: &'a mut CompilerContext<'a>, map: ModuleMap) -> Self {
        Self {
            ctx,
            map,
            scopes: ScopeManager::new(0),
            file: 0,
        }
    }

    pub fn resolve_names(&mut self) {
        for module in self.map.modules.clone() {
            debug!("AST validating module: {:?}", module.barrel.file_id);

            self.file = module.barrel.file_id;
            for mut item in module.barrel.items {
                self.visit_item(&mut item);
            }
        }
    }

    /// declare_param doesn't check for duplicates, because that is already handled by the [`AstValidator`](crate::validator::AstValidator)
    fn declare_param(&mut self, name: &str, info: VariableInfo) {
        diag_opt!(self.ctx, self.scopes.declare_variable(name.to_string(), info, false))
    }

    fn declare_variable(&mut self, name: &str, info: VariableInfo) {
        diag_opt!(self.ctx, self.scopes.declare_variable(name.to_string(), info, true))
    }

    // Check if a variable is declared in any accessible scope
    pub fn is_variable_declared(&self, name: &str) -> bool {
        self.scopes.resolve_variable(name).is_some()
    }
}

impl<'a> AstWalker for NameResolver<'a> {
    fn visit_fn(&mut self, func: &mut FnDecl) {
        self.scopes = ScopeManager::new(self.file);

        for param in &func.sig.params {
            self.declare_param(&param.name.to_string(), VariableInfo {
                id: param.id,
                is_const: false,
                span: param.span,
            });
        }
    }
}
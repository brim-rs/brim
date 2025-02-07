mod errors;
mod expr;
mod functions;

use crate::{
    items::HirItemKind,
    transformer::{HirModule, HirModuleMap},
    ty::HirTyKind,
};
use brim_middle::temp_diag::TemporaryDiagnosticContext;
use crate::inference::scope::TypeScopeManager;
use crate::items::HirFn;

#[derive(Debug)]
pub struct TypeChecker {
    pub ctx: TemporaryDiagnosticContext,
    pub hir: HirModuleMap,
    pub mod_id: usize,
    pub scope_manager: TypeScopeManager,
    pub current_fn: Option<HirFn>,
}

impl TypeChecker {
    pub fn new(hir: HirModuleMap) -> Self {
        Self {
            ctx: TemporaryDiagnosticContext::new(),
            hir,
            mod_id: 0,
            scope_manager: TypeScopeManager::new(),
            current_fn: None,
        }
    }

    pub fn check(&mut self) {
        for module in self.hir.modules.clone() {
            self.mod_id = module.mod_id.as_usize();
            self.check_module(module);
        }
    }

    pub fn check_module(&mut self, module: HirModule) {
        for item in module.items {
            match item.kind {
                HirItemKind::Fn(func) => self.check_fn(func),
                _ => {}
            }
        }
    }
}

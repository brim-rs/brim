mod errors;
mod expr;
mod functions;

use crate::{
    CompiledModules,
    inference::scope::TypeScopeManager,
    items::{HirFn, HirItemKind},
    transformer::{HirModule, HirModuleMap},
    ty::HirTyKind,
};
use brim_middle::temp_diag::TemporaryDiagnosticContext;

#[derive(Debug)]
pub struct TypeChecker {
    pub ctx: TemporaryDiagnosticContext,
    pub hir: HirModuleMap,
    pub mod_id: usize,
    pub scope_manager: TypeScopeManager,
    pub current_fn: Option<HirFn>,
    pub compiled: CompiledModules,
    pub ty_returned_from_fn: Option<HirTyKind>,
}

impl TypeChecker {
    pub fn new(hir: HirModuleMap, compiled: CompiledModules) -> Self {
        Self {
            ctx: TemporaryDiagnosticContext::new(),
            hir,
            mod_id: 0,
            scope_manager: TypeScopeManager::new(),
            current_fn: None,
            compiled,
            ty_returned_from_fn: None,
        }
    }

    pub fn check(&mut self) {
        for module in self.hir.modules.clone() {
            self.mod_id = module.mod_id.as_usize();
            self.check_module(module);
        }
    }

    pub fn check_item(&mut self, item: HirItemKind) {
        match item {
            HirItemKind::Fn(func) => self.check_fn(func),
            HirItemKind::External(external) => {
                for item in external.items {
                    let item = self.compiled.get_item(item).clone();
                    self.check_item(item.kind);
                }
            }
            HirItemKind::Struct(str) => {
                for (_, item) in str.items {
                    let item = self.compiled.get_item(item).clone();
                    self.check_item(item.kind);
                }
            }
            HirItemKind::Enum(en) => {
                for (_, item) in en.items {
                    let item = self.compiled.get_item(item).clone();
                    self.check_item(item.kind);
                }
            }
            HirItemKind::Namespace(_) | HirItemKind::Use(_) | HirItemKind::TypeAlias(_) => {}
            _ => todo!("missing implementation for {:?}", item),
        }
    }

    pub fn check_module(&mut self, module: HirModule) {
        for item in module.items {
            let item = self.compiled.get_item(item).clone();

            self.check_item(item.kind);
        }
    }
}

mod errors;
mod expr;
mod functions;

use crate::{
    MainContext,
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
    pub main_ctx: MainContext,
    pub ty_returned_from_fn: Option<HirTyKind>,
}

impl TypeChecker {
    pub fn new(hir: HirModuleMap, main_ctx: MainContext) -> Self {
        Self {
            ctx: TemporaryDiagnosticContext::new(),
            hir,
            mod_id: 0,
            scope_manager: TypeScopeManager::new(),
            current_fn: None,
            main_ctx,
            ty_returned_from_fn: None,
        }
    }

    pub fn check(&mut self) {
        for module in self.hir.modules().clone() {
            self.mod_id = module.mod_id.as_usize();
            self.check_module(module);
        }
    }

    pub fn check_item(&mut self, item: HirItemKind) {
        match item {
            HirItemKind::Fn(func) => self.check_fn(func),
            HirItemKind::External(external) => {
                for item in external.items {
                    let item = self.main_ctx.get_item(item).clone();
                    self.check_item(item.kind);
                }
            }
            HirItemKind::Struct(str) => {
                for (_, item) in str.items {
                    let item = self.main_ctx.get_item(item).clone();
                    self.check_item(item.kind);
                }
            }
            HirItemKind::Enum(en) => {
                for (_, item) in en.items {
                    let item = self.main_ctx.get_item(item).clone();
                    self.check_item(item.kind);
                }
            }
            HirItemKind::Namespace(_)
            | HirItemKind::Use(_)
            | HirItemKind::TypeAlias(_)
            | HirItemKind::EnumVariant(_) => {}
        }
    }

    pub fn check_module(&mut self, module: HirModule) {
        for item in module.items {
            let item = self.main_ctx.get_item(item).clone();

            self.check_item(item.kind);
        }
    }
}

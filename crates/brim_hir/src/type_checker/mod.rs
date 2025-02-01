mod expr;
mod functions;

use crate::{
    items::HirItemKind,
    transformer::{HirModule, HirModuleMap},
};
use brim_middle::temp_diag::TemporaryDiagnosticContext;
use crate::ty::HirTyKind;

#[derive(Debug)]
pub struct TypeChecker {
    pub ctx: TemporaryDiagnosticContext,
    pub hir: HirModuleMap,
}

impl TypeChecker {
    pub fn new(hir: HirModuleMap) -> Self {
        Self {
            ctx: TemporaryDiagnosticContext::new(),
            hir,
        }
    }

    pub fn check(&mut self) {
        for module in self.hir.modules.clone() {
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

    pub fn possible_ty_error(&mut self, ty: &HirTyKind) {
        if let HirTyKind::Err(err) = ty {
            self.ctx.emit_diag(err.clone())
        }
    }
}

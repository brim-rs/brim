use crate::{
    expr::{HirExpr, HirExprKind},
    type_checker::{TypeChecker, errors::FunctionParameterTypeMismatch},
};
use brim_middle::ModuleId;

impl TypeChecker {
    pub fn check_expr(&mut self, expr: HirExpr) {
        match expr.kind {
            HirExprKind::Block(block) => {
                self.scope_manager.push_scope();
                for stmt in block.stmts {
                    self.check_stmt(stmt);
                }
                self.scope_manager.pop_scope();
            }
            HirExprKind::Call(func, args) => {
                let ident = func.as_ident().unwrap().to_string();
                let func = self
                    .hir
                    .resolve_symbol(&ident, ModuleId::from_usize(self.mod_id))
                    .unwrap()
                    .as_fn();

                for arg in args.iter().zip(func.sig.params.params.iter()) {
                    let (arg_expr, arg_ty) = arg;
                    if arg_expr.ty != arg_ty.ty.kind {
                        self.ctx.emit_impl(FunctionParameterTypeMismatch {
                            span: (expr.span, self.mod_id),
                            name: ident.clone(),
                            expected: arg_ty.ty.kind.clone(),
                            found: arg_expr.ty.clone(),
                        });
                    }
                }
            }
            _ => {}
        }
    }
}

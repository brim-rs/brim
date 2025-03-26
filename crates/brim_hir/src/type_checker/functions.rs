use brim_ast::ty::PrimitiveType;

use crate::{
    inference::scope::TypeInfo,
    items::HirFn,
    stmts::{HirStmt, HirStmtKind},
    ty::HirTyKind,
    type_checker::{
        TypeChecker,
        errors::{CannotInitializeVariable, NoReturnFound},
    },
};

use super::errors::CannotInitializeWithVoid;

impl TypeChecker {
    pub fn check_fn(&mut self, func: HirFn) {
        if let Some(body) = func.body {
            let body = self.hir.get_expr(body);

            self.current_fn = Some(func.clone());
            self.check_expr(body.clone());
            self.current_fn = None;

            if let None = &self.ty_returned_from_fn {
                if !func.sig.return_type.can_be_ignored() {
                    self.ctx.emit_impl(NoReturnFound {
                        span: (func.sig.span, self.mod_id),
                        name: func.sig.name.to_string(),
                        expected: func.sig.return_type.clone(),
                    });
                }
            }

            self.ty_returned_from_fn = None;
        }
    }

    pub fn current_fn(&self) -> &HirFn {
        self.current_fn.as_ref().unwrap()
    }

    pub fn check_stmt(&mut self, stmt: HirStmt) {
        match stmt.kind {
            HirStmtKind::Expr(expr) => self.check_expr(expr),
            HirStmtKind::Let { ty, value, ident } => {
                let ty = ty.unwrap();

                if let Some(val) = value {
                    let val_ty = val.ty.clone();

                    if val.ty == HirTyKind::Primitive(PrimitiveType::Void) {
                        self.ctx.emit_impl(CannotInitializeWithVoid {
                            span: (stmt.span, self.mod_id),
                        });
                    } else {
                        if !ty.can_be_initialized_with(&val_ty) {
                            self.ctx.emit_impl(CannotInitializeVariable {
                                span: (stmt.span, self.mod_id),
                                name: ident.to_string(),
                                ty: ty.clone(),
                                val_ty,
                            });
                        }
                    }

                    self.check_expr(val);
                }

                self.scope_manager.declare_variable(
                    ident.to_string(),
                    TypeInfo {
                        ty,
                        span: stmt.span,
                    },
                    true,
                );
            }
        }
    }
}

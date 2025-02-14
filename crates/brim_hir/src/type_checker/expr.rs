use crate::{
    expr::{HirExpr, HirExprKind},
    items::HirGenericKind,
    ty::HirTyKind,
    type_checker::{
        TypeChecker,
        errors::{
            ExpectedResultVariant, FieldMismatch, FunctionParameterTypeMismatch,
            FunctionReturnTypeMismatch,
        },
    },
};
use brim_middle::ModuleId;
use brim_span::span::Span;

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
            HirExprKind::If(if_expr) => {
                self.check_expr(*if_expr.condition);
                self.check_expr(*if_expr.then_block);

                if let Some(else_block) = if_expr.else_block {
                    self.check_expr(*else_block);
                }

                for branch in if_expr.else_ifs {
                    self.check_expr(*branch.condition);
                    self.check_expr(*branch.block);
                }
            }
            HirExprKind::Call(func, args, call_params) => {
                let ident = func.as_ident().unwrap().to_string();
                let func = self
                    .hir
                    .resolve_symbol(&ident, ModuleId::from_usize(self.mod_id))
                    .unwrap()
                    .as_fn();

                for (arg_expr, arg_ty) in args.iter().zip(call_params) {
                    if arg_expr.ty != arg_ty.ty {
                        self.ctx.emit_impl(FunctionParameterTypeMismatch {
                            span: (expr.span, self.mod_id),
                            name: ident.clone(),
                            expected: arg_ty.ty.clone(),
                            found: arg_expr.ty.clone(),
                        });
                    }
                }
            }
            HirExprKind::Return(expr) => {
                let func = self.current_fn();
                let ret_ty = &func.sig.return_type;

                if let Some((ok, err)) = ret_ty.is_result() {
                    if let Some(ok_ty) = expr.ty.is_ok_variant() {
                        let any = if let Some(_) = func.sig.generics.is_generic(&ok) {
                            true
                        } else {
                            false
                        };

                        if ok != ok_ty && !any {
                            self.ctx.emit_impl(ExpectedResultVariant {
                                span: (expr.span, self.mod_id),
                                ok: *ok,
                                found: *ok_ty,
                                variant: "Ok".to_string(),
                            });
                        }
                    } else if let Some(err_ty) = expr.ty.is_err_variant() {
                        let any = if let Some(_) = func.sig.generics.is_generic(&err) {
                            true
                        } else {
                            false
                        };

                        if err != err_ty && !any {
                            self.ctx.emit_impl(ExpectedResultVariant {
                                span: (expr.span, self.mod_id),
                                ok: *err,
                                found: *err_ty,
                                variant: "Err".to_string(),
                            });
                        }
                    } else {
                        self.mismatch(
                            expr.span,
                            ret_ty.clone(),
                            expr.ty.clone(),
                            func.sig.name.to_string(),
                        );
                    }
                } else {
                    if ret_ty != &expr.ty {
                        self.mismatch(
                            expr.span,
                            ret_ty.clone(),
                            expr.ty.clone(),
                            func.sig.name.to_string(),
                        );
                    }
                }

                self.check_expr(*expr);
            }
            HirExprKind::StructConstructor(hir_struct) => {
                let fields = hir_struct.fields;
                let ident = hir_struct.name.to_string();

                let str = self
                    .hir
                    .resolve_symbol(&ident.to_string(), ModuleId::from_usize(self.mod_id))
                    .unwrap()
                    .as_struct()
                    .clone();

                for (ident, field) in fields {
                    let field_ty = hir_struct.field_types.get(&ident).unwrap().clone();

                    if field.ty != field_ty {
                        self.ctx.emit_impl(FieldMismatch {
                            span: (field.span, self.mod_id),
                            field: ident.to_string(),
                            expected: field_ty,
                            found: field.ty,
                        });
                    }
                }
            }
            _ => {}
        }
    }

    fn mismatch(&mut self, span: Span, expected: HirTyKind, found: HirTyKind, name: String) {
        self.ctx.emit_impl(FunctionParameterTypeMismatch {
            span: (span, self.mod_id),
            name,
            expected,
            found,
        });
    }
}

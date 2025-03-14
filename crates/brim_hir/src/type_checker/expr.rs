use crate::{
    expr::{HirExpr, HirExprKind},
    ty::HirTyKind,
    type_checker::{
        TypeChecker,
        errors::{
            AssignMismatch, FieldMismatch, FunctionParameterTypeMismatch,
            FunctionReturnTypeMismatch,
        },
    },
};
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

                for (arg_expr, arg_ty) in args.iter().zip(call_params) {
                    if arg_ty.ty != arg_expr.ty {
                        self.ctx.emit_impl(FunctionParameterTypeMismatch {
                            span: (arg_expr.span, self.mod_id),
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

                if ret_ty != &expr.ty {
                    self.ctx.emit_impl(FunctionReturnTypeMismatch {
                        span: (expr.span, self.mod_id),
                        expected: ret_ty.clone(),
                        found: expr.ty.clone(),
                        name: func.sig.name.to_string(),
                    });
                }

                self.check_expr(*expr);
            }
            HirExprKind::StructConstructor(hir_struct) => {
                let fields = hir_struct.fields;

                for (ident, field) in fields {
                    let field_ty = hir_struct.field_types.get(&ident).unwrap().clone();

                    if field_ty != field.ty {
                        self.ctx.emit_impl(FieldMismatch {
                            span: (field.span, self.mod_id),
                            field: ident.to_string(),
                            expected: field_ty,
                            found: field.ty,
                        });
                    }
                }
            }
            HirExprKind::Assign(lhs, rhs) => {
                if lhs.ty != rhs.ty {
                    self.ctx.emit_impl(AssignMismatch {
                        span: (lhs.span.to(rhs.span), self.mod_id),
                        expected: lhs.ty.clone(),
                        found: rhs.ty.clone(),
                    });
                }

                self.check_expr(*lhs);
                self.check_expr(*rhs);
            }
            _ => {}
        }
    }

    fn param_mismatch(&mut self, span: Span, expected: HirTyKind, found: HirTyKind, name: String) {
        self.ctx.emit_impl(FunctionParameterTypeMismatch {
            span: (span, self.mod_id),
            name,
            expected,
            found,
        });
    }
}

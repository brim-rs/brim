mod errors;
mod scope;

use crate::{
    expr::{HirExpr, HirExprKind}, inference::{
        errors::{CannotApplyBinary, CannotApplyUnary, CannotCompare},
        scope::{TypeInfo, TypeScopeManager},
    }, items::{HirCallParam, HirGenericKind, HirGenericParam, HirItem, HirItemKind, HirParam}, stmts::{HirStmt, HirStmtKind}, transformer::{HirModule, HirModuleMap, StoredHirItem}, ty::HirTyKind, HirId
};
use brim_ast::{
    expr::{BinOpKind, UnaryOp},
    token::{Lit, LitKind},
    ty::PrimitiveType,
};
use brim_diagnostics::diagnostic::ToDiagnostic;
use brim_middle::{ModuleId, temp_diag::TemporaryDiagnosticContext};

#[derive(Debug)]
pub struct TypeInference<'a> {
    pub hir: &'a mut HirModuleMap,
    pub ctx: InferCtx,
    pub scope_manager: TypeScopeManager,
    pub current_mod: ModuleId,
    pub temp: TemporaryDiagnosticContext,
}

#[derive(Debug, Clone)]
pub struct InferCtx {
    /// Generics available in the current scope.
    pub generics: Vec<HirGenericParam>,
}

impl InferCtx {
    pub fn new() -> Self {
        Self {
            generics: Vec::new(),
        }
    }

    pub fn push_generic(&mut self, generic: HirGenericParam) {
        self.generics.push(generic);
    }

    pub fn clear_generics(&mut self) {
        self.generics.clear();
    }
}

pub fn infer_types(hir: &mut HirModuleMap) -> TypeInference {
    let mut ti = TypeInference {
        hir,
        ctx: InferCtx::new(),
        scope_manager: TypeScopeManager::new(),
        current_mod: ModuleId::from_usize(0),
        temp: TemporaryDiagnosticContext::new(),
    };
    ti.infer();

    ti
}

impl<'a> TypeInference<'a> {
    pub fn infer(&mut self) {
        let inferred_modules: Vec<_> = self
            .hir
            .modules
            .clone()
            .into_iter()
            .map(|mut module| {
                self.current_mod = module.mod_id;
                self.infer_module(&mut module);
                module
            })
            .collect();

        self.hir.modules = inferred_modules;
    }

    pub fn infer_module(&mut self, module: &mut HirModule) {
        for item in &mut module.items {
            self.infer_item(item);
        }
    }

    fn infer_item(&mut self, item: &mut HirItem) {
        match &mut item.kind {
            HirItemKind::Fn(f) => {
                self.scope_manager.push_scope();

                self.ctx.clear_generics();
                for generic in &f.sig.generics.params {
                    self.ctx.push_generic(generic.clone());
                }

                for param in &f.sig.params.params {
                    let param_type = param.ty.kind.clone();
                    let type_info = TypeInfo {
                        ty: param_type,
                        span: param.span.clone(),
                    };
                    self.scope_manager.declare_variable(
                        param.name.clone().to_string(),
                        type_info,
                        true,
                    );
                }


                f.ret_type = f
                    .sig
                    .return_type
                    .as_ref()
                    .map_or(HirTyKind::Primitive(PrimitiveType::Void), |ty| {
                        ty.kind.clone()
                    });

                if let Some(body_id) = f.body {
                    self.infer_body(body_id);
                }

                self.scope_manager.pop_scope(); // Reset scope after function
            }
            _ => {}
        }
    }

    fn infer_body(&mut self, body_id: HirId) {
        let mut expr = self.hir.get_expr_mut(body_id).clone();

        self.infer_expr(&mut expr);
        *self.hir.get_expr_mut(body_id) = expr;
    }

    fn infer_stmt(&mut self, stmt: &mut HirStmt) {
        match &mut stmt.kind {
            HirStmtKind::Expr(expr) => self.infer_expr(expr),
            HirStmtKind::Let { ty, value, ident } => {
                if let Some(value) = value {
                    self.infer_expr(value);

                    if let None = ty {
                        ty.replace(value.ty.clone());
                    }
                }

                self.scope_manager.declare_variable(
                    ident.to_string(),
                    TypeInfo {
                        ty: ty.clone().unwrap(),
                        span: stmt.span.clone(),
                    },
                    true,
                );
            }
        }
    }

    fn is_generic(&self, ty: &HirTyKind) -> Option<HirGenericParam> {
        if let HirTyKind::Ident { ident, .. } = ty {
            self.ctx
                .generics
                .iter()
                .find(|g| g.name.to_string() == *ident.to_string())
                .cloned()
        } else {
            None
        }
    }

    fn assign_generic(&self, expr: &mut HirExpr) {
        if let Some(generic) = self.is_generic(&expr.ty) {
            println!("{:#?}", generic);
            if let HirGenericKind::Type {
                default: Some(default),
            } = generic.kind
            {
                expr.ty = default.kind;
            } else {
                expr.ty = HirTyKind::Placeholder;
            }
        }
    }

    fn infer_expr(&mut self, expr: &mut HirExpr) {
        let kind = match &mut expr.kind {
            HirExprKind::Unary(op, operand) => {
                self.infer_expr(operand);

                match (op, &operand.ty) {
                    (UnaryOp::Try, ty) => ty,
                    // Unary minus only applies to numeric types. We assign err, that will be later emitted in the type checker. Same goes for the rest of the cases.
                    (UnaryOp::Minus, ty) => {
                        if ty.is_numeric() {
                            ty
                        } else {
                            &self.ret_with_error(CannotApplyUnary {
                                span: (expr.span.clone(), self.current_mod.as_usize()),
                                op: UnaryOp::Minus,
                                ty: ty.clone(),
                            })
                        }
                    }
                    (UnaryOp::Deref, ty) => {
                        if ty.can_be_dereferenced() {
                            ty
                        } else {
                            &self.ret_with_error(CannotApplyUnary {
                                span: (expr.span.clone(), self.current_mod.as_usize()),
                                op: UnaryOp::Deref,
                                ty: ty.clone(),
                            })
                        }
                    }
                    (UnaryOp::Not, _) => &HirTyKind::Primitive(PrimitiveType::Bool),
                }
            }
            HirExprKind::Block(block) => &{
                for stmt in &mut block.stmts {
                    self.infer_stmt(stmt);
                }

                block
                    .stmts
                    .last()
                    .and_then(|stmt| stmt.can_be_used_for_inference())
                    .unwrap_or(HirTyKind::void())
            },
            HirExprKind::Return(expr) => &{
                self.infer_expr(expr);
                expr.ty.clone()
            },
            HirExprKind::Binary(lhs, op, rhs) => {
                self.infer_expr(lhs);
                self.infer_expr(rhs);

                // The generics will be inferred later, so we can't infer the type yet.
                self.assign_generic(lhs);
                self.assign_generic(rhs);

                let opc = op.clone();
                match (op, &lhs.ty, &rhs.ty) {
                    // Numeric operations
                    (
                        BinOpKind::Plus
                        | BinOpKind::Minus
                        | BinOpKind::Multiply
                        | BinOpKind::Divide
                        | BinOpKind::Modulo
                        | BinOpKind::Power
                        | BinOpKind::Caret
                        | BinOpKind::ShiftRight
                        | BinOpKind::ShiftLeft
                        | BinOpKind::And
                        | BinOpKind::Or,
                        l,
                        r,
                    ) => {
                        if l.is_numeric() && r.is_numeric() {
                            let ty =
                                PrimitiveType::promote_type(&l.to_primitive(), &r.to_primitive())
                                    .unwrap();

                            &HirTyKind::Primitive(ty)
                        } else {
                            &self.ret_with_error(CannotApplyBinary {
                                span: (expr.span.clone(), self.current_mod.as_usize()),
                                op: opc,
                                lhs: l.clone(),
                                rhs: r.clone(),
                            })
                        }
                    }

                    (
                        BinOpKind::Lt
                        | BinOpKind::Le
                        | BinOpKind::EqEq
                        | BinOpKind::Ne
                        | BinOpKind::Ge
                        | BinOpKind::Gt,
                        l,
                        r,
                    ) => {
                        if l.can_be_logically_compared_to(r) {
                            &HirTyKind::Primitive(PrimitiveType::Bool)
                        } else {
                            &self.ret_with_error(CannotCompare {
                                span: (expr.span.clone(), self.current_mod.as_usize()),
                                op: opc,
                                lhs: l.clone(),
                                rhs: r.clone(),
                            })
                        }
                    }

                    (BinOpKind::AndAnd | BinOpKind::OrOr, l, r) => {
                        if l.is_bool() && r.is_bool() {
                            &HirTyKind::Primitive(PrimitiveType::Bool)
                        } else {
                            &self.ret_with_error(CannotApplyUnary {
                                span: (expr.span.clone(), self.current_mod.as_usize()),
                                op: UnaryOp::Not,
                                ty: l.clone(),
                            })
                        }
                    }
                }
            }
            HirExprKind::Var(name) => {
                let var = self.scope_manager.resolve_variable(&name.to_string());

                if let Some(var) = var {
                    &var.ty
                } else {
                    // &HirTyKind::err()
                    panic!()
                }
            }
            HirExprKind::Call(ident, args, params) => {
                let func_ident = ident.as_ident().unwrap().to_string();
                let func = self.hir.resolve_symbol(&func_ident, self.current_mod).cloned();

                // TODO: take into account provided generic arguments
                if let Some(func) = func {
                    let func_params = func.as_fn().sig.params.params.clone();
                    for (args, param) in args.iter_mut().zip(func_params) {
                        self.infer_expr(args);

                        self.assign_generic(args);
                        params.push(HirCallParam {
                                span: param.span.clone(),
                                name: func.ident.to_owned(),
                                ty: args.ty.clone(),
                        });
                    }

                    ret_type
                } else {
                    &HirTyKind::Placeholder
                }
            }
            HirExprKind::Literal(lit) => match lit.kind {
                LitKind::Str => &HirTyKind::Primitive(PrimitiveType::Str),
                LitKind::Byte => &HirTyKind::Primitive(PrimitiveType::U8),
                // TODO: should be array, but we don't really have way to transform the length to a constant expression. Maybe we will evaluate it before.
                LitKind::ByteStr => {
                    &HirTyKind::Vec(Box::new(HirTyKind::Primitive(PrimitiveType::U8)))
                }
                LitKind::Char => &HirTyKind::Primitive(PrimitiveType::Char),
                LitKind::Bool => &HirTyKind::Primitive(PrimitiveType::Bool),
                LitKind::Integer => &self.ty_with_suffix(lit, PrimitiveType::I32),
                LitKind::Float => &self.ty_with_suffix(lit, PrimitiveType::F64),
                LitKind::CStr => todo!("CStr"),
                _ => unreachable!("literal error: {:#?}", lit),
            },
            HirExprKind::If(if_expr) => {
                self.infer_expr(&mut if_expr.condition);
                self.infer_expr(&mut if_expr.then_block);

                if let Some(else_block) = &mut if_expr.else_block {
                    self.infer_expr(else_block);
                }

                for branch in &mut if_expr.else_ifs {
                    self.infer_expr(&mut branch.condition);
                    self.infer_expr(&mut branch.block);
                }

                // For now, we will just use placeholder until we figure out how to compare types.
                &HirTyKind::Placeholder
            }
            _ => todo!("infer_expr: {:?}", expr.kind),
        };
        expr.ty = kind.clone();

        self.hir
            .hir_items
            .insert(expr.id, StoredHirItem::Expr(expr.clone()));
    }

    pub fn ty_with_suffix(&mut self, lit: &Lit, def: PrimitiveType) -> HirTyKind {
        if let Some(suf) = lit.suffix {
            let ty = PrimitiveType::try_from_string(suf.to_string()).unwrap();

            HirTyKind::Primitive(ty)
        } else {
            HirTyKind::Primitive(def)
        }
    }

    pub fn ret_with_error(&mut self, err: impl ToDiagnostic + 'static) -> HirTyKind {
        self.temp.emit(Box::new(err));
        HirTyKind::err()
    }
}

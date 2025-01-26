mod scope;

use crate::{
    HirId,
    expr::{HirExpr, HirExprKind},
    inference::scope::{TypeInfo, TypeScopeManager},
    items::{HirGenericKind, HirGenericParam, HirItem, HirItemKind},
    stmts::{HirStmt, HirStmtKind},
    transformer::{HirModule, HirModuleMap},
    ty::HirTyKind,
};
use brim_ast::{
    expr::{BinOpKind, UnaryOp},
    ty::PrimitiveType,
};
use brim_ctx::ModuleId;

#[derive(Debug)]
pub struct TypeInference<'a> {
    pub hir: &'a mut HirModuleMap,
    pub ctx: InferCtx,
    pub scope_manager: TypeScopeManager,
    pub current_mod: ModuleId,
}

#[derive(Debug)]
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

pub fn infer_types(hir: &mut HirModuleMap) {
    let ti = &mut TypeInference {
        hir,
        ctx: InferCtx::new(),
        scope_manager: TypeScopeManager::new(),
        current_mod: ModuleId::from_usize(0),
    };

    ti.infer();
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

                for param in &f.sig.params {
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

                self.ctx.clear_generics();
                for generic in &f.sig.generics.params {
                    self.ctx.push_generic(generic.clone());
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
                if let None = ty {
                    if let Some(value) = value {
                        self.infer_expr(value);
                        *ty = Some(value.ty.clone());
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
                            &HirTyKind::err()
                        }
                    }
                    (UnaryOp::Deref, ty) => {
                        if ty.can_be_dereferenced() {
                            ty
                        } else {
                            &HirTyKind::err()
                        }
                    }
                    (UnaryOp::Not, ty) => &HirTyKind::Primitive(PrimitiveType::Bool),
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
                            &HirTyKind::err()
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
                            &HirTyKind::err()
                        }
                    }

                    (BinOpKind::AndAnd | BinOpKind::OrOr, l, r) => {
                        if l.is_bool() && r.is_bool() {
                            &HirTyKind::Primitive(PrimitiveType::Bool)
                        } else {
                            &HirTyKind::err()
                        }
                    }
                }
            }
            HirExprKind::Var(name) => {
                let var = self
                    .scope_manager
                    .resolve_variable(&name.to_string())
                    .unwrap();
                &var.ty
            }
            HirExprKind::Call(ident, args) => {
                let func = self
                    .hir
                    // we unwrap, because this was already checked in the name resolver
                    .resolve_symbol(&ident.as_ident().unwrap().to_string(), self.current_mod)
                    .unwrap();

                &func.as_fn().ret_type
            }
            _ => todo!("infer_expr: {:?}", expr.kind),
        };

        expr.ty = kind.clone();
    }
}

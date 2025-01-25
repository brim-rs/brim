use brim_ast::ErrorEmitted;
use brim_ast::expr::{BinOpKind, UnaryOp};
use brim_ast::ty::PrimitiveType;
use crate::expr::{HirExpr, HirExprKind};
use crate::HirId;
use crate::items::{HirGenericParam, HirItem, HirItemKind};
use crate::stmts::{HirStmt, HirStmtKind};
use crate::transformer::{HirModule, HirModuleMap};
use crate::ty::{HirTy, HirTyKind};

#[derive(Debug)]
pub struct TypeInference<'a> {
    pub hir: &'a mut HirModuleMap,
    pub ctx: InferCtx,
}

#[derive(Debug)]
pub struct InferCtx {
    /// Generics available in the current scope.
    pub generics: Vec<HirGenericParam>,
}

impl InferCtx {
    pub fn new() -> Self {
        Self { generics: Vec::new() }
    }

    pub fn push_generic(&mut self, generic: HirGenericParam) {
        self.generics.push(generic);
    }

    pub fn clear_generics(&mut self) {
        self.generics.clear();
    }
}

pub fn infer_types(hir: &mut HirModuleMap) {
    let ti = &mut TypeInference { hir, ctx: InferCtx::new() };

    ti.infer();
}

impl<'a> TypeInference<'a> {
    pub fn infer(&mut self) {
        let modules = std::mem::take(&mut self.hir.modules);

        self.hir.modules = modules.into_iter()
            .map(|mut module| {
                self.infer_module(&mut module);
                module
            })
            .collect();
    }

    pub fn infer_module(&mut self, module: &mut HirModule) {
        for item in &mut module.items {
            self.infer_item(item);
        }
    }

    fn infer_item(&mut self, item: &mut HirItem) {
        match &mut item.kind {
            HirItemKind::Fn(f) => {
                self.ctx.clear_generics();
                for generic in &f.sig.generics.params {
                    self.ctx.push_generic(generic.clone());
                }

                f.ret_type = f.sig.return_type
                    .as_ref()
                    .map_or(HirTyKind::Primitive(PrimitiveType::Void), |ty| ty.kind.clone());

                if let Some(body_id) = f.body {
                    self.infer_body(body_id);
                }
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
            HirStmtKind::Let { ty, value, .. } => {
                if let None = ty {
                    if let Some(value) = value {
                        *ty = Some(value.ty.clone());
                    }
                }
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
                    (UnaryOp::Minus, ty) => if ty.is_numeric() { ty } else { &HirTyKind::err() },
                    (UnaryOp::Deref, ty) => if ty.can_be_dereferenced() { ty } else { &HirTyKind::err() },
                    (UnaryOp::Not, ty) => &HirTyKind::Primitive(PrimitiveType::Bool),
                }
            }
            HirExprKind::Block(block) => &{
                for stmt in &mut block.stmts {
                    self.infer_stmt(stmt);
                }

                block.stmts.last().and_then(|stmt| stmt.can_be_used_for_inference())
                    .unwrap_or(HirTyKind::void())
            },
            HirExprKind::Return(expr) => &{
                self.infer_expr(expr);
                expr.ty.clone()
            },
            _ => todo!("infer_expr: {:?}", expr.kind),
        };

        expr.ty = kind.clone();
    }
}
pub mod errors;
mod eval_scopes;

use crate::{
    comptime::{
        errors::NoValueReturned,
        eval_scopes::{EvalScopeManager, VariableInfo},
    },
    expr::{HirBlock, HirExpr, HirExprKind},
    stmts::HirStmtKind,
    transformer::Transformer,
    ty::HirTy,
};
use brim_ast::{
    Empty,
    expr::Expr,
    token::{Lit, LitKind},
};
use brim_middle::{ModuleId, temp_diag::TemporaryDiagnosticContext};

#[derive(Debug, Clone)]
pub enum ComptimeReturnValue {
    Lit(Lit),
    Ty(HirTy),
}

impl ComptimeReturnValue {
    pub fn as_lit(&self) -> &Lit {
        match self {
            ComptimeReturnValue::Lit(lit) => lit,
            _ => unreachable!(),
        }
    }

    pub fn as_ty(&self) -> &HirTy {
        match self {
            ComptimeReturnValue::Ty(ty) => ty,
            _ => unreachable!(),
        }
    }
}

impl Transformer {
    pub fn transform_comptime_expr(&mut self, expr: Expr) -> ComptimeReturnValue {
        let (expr, _) = self.transform_expr(expr);

        Evaluator::new(self.current_mod_id).eval_expr(expr)
    }
}

/// For now only literals will be able to return from comptime expressions.
#[derive(Debug)]
pub struct Evaluator {
    pub scopes: EvalScopeManager,
    pub last_val: Option<ComptimeReturnValue>,
    pub mod_id: usize,
    pub temp: TemporaryDiagnosticContext,
}

impl Evaluator {
    pub fn new(mod_id: ModuleId) -> Self {
        Self {
            scopes: EvalScopeManager::new(0),
            last_val: None,
            mod_id: mod_id.as_usize(),
            temp: TemporaryDiagnosticContext::new(),
        }
    }
}

impl Evaluator {
    pub fn eval_block(&mut self, block: &HirBlock) -> ComptimeReturnValue {
        self.scopes.push_scope(self.mod_id);
        for stmt in block.stmts.clone() {
            match stmt.kind {
                HirStmtKind::Expr(expr) => {
                    self.eval_expr(expr);
                }
                HirStmtKind::Let { value, ident, .. } => {
                    let val = if let Some(val) = value {
                        self.eval_expr(val)
                    } else {
                        ComptimeReturnValue::Lit(Lit::new(LitKind::None, Empty, None))
                    };
                    self.scopes
                        .declare_variable(ident.name.to_string(), VariableInfo {
                            span: ident.span,
                            val,
                        });
                }
            };
        }
        self.scopes.pop_scope();

        if let Some(val) = self.last_val.clone() {
            val
        } else {
            let err = self.temp.emit_impl(NoValueReturned {
                span: (block.span, self.mod_id),
            });

            ComptimeReturnValue::Lit(Lit::new(LitKind::Err(err), Empty, None))
        }
    }

    pub fn eval_expr(&mut self, expr: HirExpr) -> ComptimeReturnValue {
        let lit = match &expr.kind {
            HirExprKind::Literal(lit) => ComptimeReturnValue::Lit(lit.clone()),
            HirExprKind::Block(block) => self.eval_block(block),
            _ => todo!(),
        };

        self.last_val = Some(lit.clone());
        lit
    }
}

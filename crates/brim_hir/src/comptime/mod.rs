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
    ty::HirTyKind,
};
use brim_ast::{
    Empty,
    expr::Expr,
    token::{Lit, LitKind},
    ty::PrimitiveType,
};
use brim_diagnostics::ErrorEmitted;
use brim_middle::{ModuleId, temp_diag::TemporaryDiagnosticContext};
use brim_span::symbols::Symbol;

impl Transformer {
    pub fn transform_comptime_expr(&mut self, expr: Expr) -> Lit {
        let (expr, _) = self.transform_expr(expr);

        let lit = Evaluator::new(self.current_mod_id).eval_block(expr.as_block());

        println!("{:?}", lit);

        lit
    }
}

/// For now only literals will be able to return from comptime expressions.
#[derive(Debug)]
pub struct Evaluator {
    pub scopes: EvalScopeManager,
    pub last_val: Option<Lit>,
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
    pub fn eval_block(&mut self, block: &HirBlock) -> Lit {
        self.scopes.push_scope(self.mod_id);
        for stmt in block.stmts.clone() {
            match stmt.kind {
                HirStmtKind::Expr(expr) => {
                    self.eval_expr(expr);
                }
                HirStmtKind::Let { value, ident, .. } => {
                    let lit = if let Some(val) = value {
                        self.eval_expr(val)
                    } else {
                        Lit::new(LitKind::None, Empty, None)
                    };
                    self.scopes
                        .declare_variable(ident.name.to_string(), VariableInfo {
                            span: ident.span,
                            val: lit,
                        });
                }
            };
        }
        self.scopes.pop_scope();

        if let Some(val) = self.last_val {
            val
        } else {
            let err = self.temp.emit_impl(NoValueReturned {
                span: (block.span, self.mod_id),
            });

            Lit::new(LitKind::Err(err), Empty, None)
        }
    }

    pub fn eval_expr(&mut self, expr: HirExpr) -> Lit {
        let lit = match &expr.kind {
            HirExprKind::Literal(lit) => lit.clone(),
            _ => todo!(),
        };

        self.last_val = Some(lit.clone());
        lit
    }
}

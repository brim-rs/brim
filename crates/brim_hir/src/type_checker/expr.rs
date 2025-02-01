use crate::expr::{HirExpr, HirExprKind};
use crate::type_checker::TypeChecker;

impl TypeChecker {
    pub fn check_expr(&mut self, expr: HirExpr) {
        match expr.kind {
            HirExprKind::Block(block) => {
                for stmt in block.stmts {
                    self.check_stmt(stmt);
                }
            }
            _ => {}
        }
    }
}
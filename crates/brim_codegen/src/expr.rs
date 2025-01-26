use crate::codegen::CppCodegen;
use brim_hir::expr::{HirExpr, HirExprKind};

impl CppCodegen {
    pub fn generate_expr(&mut self, expr: HirExpr) -> String {
        match expr.kind {
            HirExprKind::Block(block) => {
                let mut code = String::new();
                for stmt in block.stmts {
                    code.push_str(&self.generate_stmt(stmt));
                }
                code
            } 
            HirExprKind::Return(expr) => {
                let expr = self.generate_expr(*expr);
                format!("return {};", expr)
            }
            _ => String::new()
        }
    }
}

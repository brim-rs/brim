use crate::codegen::CppCodegen;
use brim_hir::stmts::{HirStmt, HirStmtKind};

impl CppCodegen {
    pub fn generate_stmt(&mut self, stmt: HirStmt) -> String {
        match stmt.kind {
            HirStmtKind::Expr(expr) => self.generate_expr(expr),
            HirStmtKind::Let { value, ty, ident } => {
                let ty = self.generate_ty(ty.unwrap());

                if let Some(value) = value {
                    let value = self.generate_expr(value);
                    format!("{} brim_{} = {};", ty, ident.to_string(), value)
                } else {
                    format!("{} brim_{};", ty, ident.to_string())
                }
            }
            HirStmtKind::If(if_expr) => {
                let if_expr = if_expr.as_if().clone();
                
                self.generate_if_expr(if_expr, None)
            }
        }
    }
}

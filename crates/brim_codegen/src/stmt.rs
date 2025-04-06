use crate::codegen::CppCodegen;
use brim_hir::{
    expr::HirExprKind,
    stmts::{HirStmt, HirStmtKind},
};

impl CppCodegen {
    pub fn generate_stmt(&mut self, stmt: HirStmt) -> String {
        match stmt.kind {
            HirStmtKind::Expr(expr) => format!("{};", self.generate_expr(expr)),
            HirStmtKind::Let { value, ty, ident } => {
                let ty = self.generate_ty(ty.unwrap());

                let ident = ident.to_string();
                if let Some(value) = value {
                    if let HirExprKind::Ternary(cond, then, else_) = value.kind {
                        let var_decl = format!("{} brim_{};", ty, ident.to_string());

                        return format!(
                            "{var_decl}\n if ({}) {{ brim_{ident} = {}; }} else {{ brim_{ident} = {}; }}",
                            self.generate_expr(*cond),
                            self.generate_expr(*then),
                            self.generate_expr(*else_),
                        );
                    }

                    let value = self.generate_expr(value);
                    format!("{} brim_{} = {};", ty, ident.to_string(), value)
                } else {
                    format!("{} brim_{};", ty, ident.to_string())
                }
            }
            HirStmtKind::If(if_expr) => self.generate_if_stmt(if_expr),
        }
    }
}

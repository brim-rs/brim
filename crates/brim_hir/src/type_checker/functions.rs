use crate::{
    items::HirFn,
    stmts::{HirStmt, HirStmtKind},
    type_checker::TypeChecker,
};
use brim_ast::stmts::Stmt;

impl TypeChecker {
    pub fn check_fn(&mut self, func: HirFn) {
        if let Some(body) = func.body {
            let body = self.hir.get_expr(body);

            self.check_expr(body.clone());
        }
    }

    pub fn check_stmt(&mut self, stmt: HirStmt) {
        match stmt.kind {
            HirStmtKind::Expr(expr) => self.check_expr(expr),
            HirStmtKind::Let { ty, value, ident } => {
                let ty = ty.unwrap();

                if let Some(val) = value {
                    let val_ty = val.ty;

                    self.possible_ty_error(ty);
                    self.possible_ty_error(val_ty);
                }
            }
        }
    }
}

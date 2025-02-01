use brim_ast::stmts::Stmt;
use crate::items::HirFn;
use crate::stmts::{HirStmt, HirStmtKind};
use crate::type_checker::TypeChecker;

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
                println!("{:#?}", ty);
                println!("{:#?}", value);
            }
        }
    }
}
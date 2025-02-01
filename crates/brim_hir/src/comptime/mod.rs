pub mod errors;

use crate::{expr::HirExprKind, transformer::Transformer, ty::HirTyKind};
use brim_ast::{expr::Expr, token::Lit, ty::PrimitiveType};

impl Transformer {
    pub fn transform_comptime_expr(&mut self, expr: Expr) -> HirExprKind {
        let (expr, _) = self.transform_expr(expr);

        todo!()
    }

    pub fn expect_from_comptime(&mut self, expr: Expr) -> Lit {
        let hir_expr = self.transform_comptime_expr(expr);

        match hir_expr {
            HirExprKind::Literal(lit) => lit,
            _ => panic!("Expected literal expression"),
        }
    }
}

#![feature(let_chains)]

use crate::{expr::HirExpr, items::HirItem, stmts::HirStmt, transformer::HirModule, ty::HirTyKind};

pub mod builtin;
pub mod comptime;
pub mod expr;
pub mod inference;
pub mod items;
pub mod stmts;
pub mod transformer;
pub mod ty;
pub mod type_checker;

pub trait Codegen {
    fn generate(&mut self);

    fn generate_module(&mut self, module: HirModule);

    fn generate_item(&mut self, item: HirItem);

    fn generate_expr(&mut self, expr: HirExpr) -> String;

    fn generate_stmt(&mut self, stmt: HirStmt) -> String;

    fn generate_ty(&mut self, ty: HirTyKind) -> String;
}

#![feature(let_chains)]

use crate::{items::HirItem, transformer::HirModule};
use brim_index::index_type;
use crate::expr::HirExpr;
use crate::stmts::HirStmt;
use crate::ty::{HirTy, HirTyKind};

pub mod builtin;
pub mod comptime;
pub mod expr;
pub mod inference;
pub mod items;
pub mod stmts;
pub mod transformer;
pub mod ty;
pub mod type_checker;

index_type! {
    /// A unique identifier for a node in the HIR.
    pub struct HirId {}
}

pub trait Codegen {
    fn generate(&mut self);

    fn generate_module(&mut self, module: HirModule);

    fn generate_item(&mut self, item: HirItem);

    fn generate_expr(&mut self, expr: HirExpr) -> String;

    fn generate_stmt(&mut self, stmt: HirStmt) -> String;

    fn generate_ty(&mut self, ty: HirTyKind) -> String;
}

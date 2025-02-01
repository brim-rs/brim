#![feature(let_chains)]

use brim_index::index_type;

mod comptime;
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

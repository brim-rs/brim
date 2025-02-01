#![feature(let_chains)]

use brim_index::index_type;

pub mod expr;
pub mod inference;
pub mod items;
pub mod stmts;
pub mod transformer;
pub mod ty;

index_type! {
    /// A unique identifier for a node in the HIR.
    pub struct HirId {}
}

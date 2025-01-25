use brim_index::index_type;

pub mod expr;
pub mod items;
pub mod stmts;
pub mod transformer;
pub mod ty;
pub mod inference;

index_type! {
    /// A unique identifier for a node in the HIR.
    pub struct HirId {}
}

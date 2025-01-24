use brim_index::index_type;

mod stmts;
mod expr;
mod transformer;
mod items;
mod ty;

index_type! {
    /// A unique identifier for a node in the HIR.
    pub struct HirId {}
}

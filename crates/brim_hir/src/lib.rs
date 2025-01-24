use brim_index::index_type;

mod expr;
mod items;
mod stmts;
mod transformer;
mod ty;

index_type! {
    /// A unique identifier for a node in the HIR.
    pub struct HirId {}
}

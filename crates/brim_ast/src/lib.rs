use brim_index::index_type;

mod expr;
pub mod item;
mod stmts;
pub mod token;
mod ty;

/// A struct that represents already emitted diagnostic
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ErrorEmitted(());

impl ErrorEmitted {
    pub fn new() -> Self {
        Self(())
    }
}

index_type! {
    /// A unique identifier for a node in the AST.
    pub struct NodeId {}
}

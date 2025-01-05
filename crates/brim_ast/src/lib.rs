use brim_index::index_type;

pub mod token;
pub mod item;
mod ty;
mod expr;
mod stmts;

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
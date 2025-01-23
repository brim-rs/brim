use brim_ast::{NodeId, item::Item};

#[derive(Clone, Debug)]
pub struct Barrel {
    pub items: Vec<Item>,
    pub id: NodeId,
    pub file_id: usize,
}

use brim_ast::{NodeId, item::Item};

#[derive(Debug)]
pub struct Barrel {
    pub items: Vec<Item>,
    pub id: NodeId,
    pub file_id: usize,
}

use brim_ast::{ItemId, item::Item};

#[derive(Clone, Debug)]
pub struct Barrel {
    pub items: Vec<Item>,
    pub id: ItemId,
    pub file_id: usize,
}

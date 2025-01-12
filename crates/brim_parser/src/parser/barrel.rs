use brim_ast::item::Item;
use brim_ast::NodeId;

#[derive(Debug)]
pub struct Barrel {
    pub items: Vec<Item>,
    pub id: NodeId,
}

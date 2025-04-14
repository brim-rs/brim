use brim_ast::{ItemId, item::Item, token::Token};
use brim_span::span::Span;

#[derive(Clone, Debug)]
pub struct Barrel {
    pub items: Vec<Item>,
    pub id: ItemId,
    pub file_id: usize,
    pub tokens: Vec<Token>,
    pub comments: Vec<Span>,
}

use crate::ast::ItemId;
use crate::ast::statements::Stmt;

#[derive(Clone, Debug, PartialEq)]
pub struct TopLevelItem {
    pub stmt: Stmt,
    pub id: ItemId,
}

impl TopLevelItem {
    pub fn new(stmt: Stmt, id: ItemId) -> Self {
        Self { stmt, id }
    }
}
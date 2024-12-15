use crate::ast::{ItemId, StmtId};
use crate::ast::statements::Stmt;

#[derive(Clone, Debug, PartialEq)]
pub struct TopLevelItem {
    pub stmt: StmtId,
    pub id: ItemId,
}

impl TopLevelItem {
    pub fn new(stmt: StmtId, id: ItemId) -> Self {
        Self { stmt, id }
    }
}

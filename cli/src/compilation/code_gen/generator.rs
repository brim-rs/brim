use crate::ast::item::TopLevelItem;
use crate::compilation::code_gen::CodeGen;
use anyhow::Result;
use crate::ast::statements::{Stmt, StmtKind};
use crate::context::GlobalContext;

impl<'a> CodeGen<'a> {
    pub fn generate_item(&mut self, item: TopLevelItem) -> Result<()> {
        let stmt = item.stmt;
        let stmt = self.unit.ast().query_stmt(stmt).clone();

        self.generate_stmt(stmt)?;

        Ok(())
    }

    pub fn generate_stmt(&mut self, stmt: Stmt) -> Result<()> {
        match stmt.kind {
            // StmtKind::Fn(function) => self.generate_fn(function)?,
            _ => {}
        }

        Ok(())
    }
}
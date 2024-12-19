use crate::{
    ast::{ItemId, StmtId},
    compilation::unit::CompilationUnit,
};

pub mod resolver;
pub mod type_checker;

pub trait Pass {
    fn visit_item(&mut self, unit: &mut CompilationUnit, item: ItemId) -> anyhow::Result<()> {
        self.visit_item_default(unit, item)?;

        Ok(())
    }

    fn visit_item_default(
        &mut self,
        unit: &mut CompilationUnit,
        item: ItemId,
    ) -> anyhow::Result<()> {
        let item = unit.ast().query_item(item).clone();

        self.visit_statement(item.stmt)?;

        Ok(())
    }

    fn visit_statement(&mut self, statement: StmtId) -> anyhow::Result<()> {
        self.do_visit_statement(statement)?;

        Ok(())
    }

    fn do_visit_statement(&mut self, statement: StmtId) -> anyhow::Result<()>;
}

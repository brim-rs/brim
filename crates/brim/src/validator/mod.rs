use crate::resolver::Resolver;
use anyhow::Result;
use tracing::debug;
use brim_ast::item::Item;
use brim_ctx::compiler::CompilerContext;
use brim_ctx::modules::ModuleMap;
use brim_ctx::walker::AstWalker;

#[derive(Debug)]
pub struct AstValidator<'a> {
    pub ctx: &'a mut CompilerContext<'a>,
}

impl<'a> AstValidator<'a> {
    pub fn new(ctx: &'a mut CompilerContext<'a>) -> Self {
        Self { ctx }
    }

    /// Validates AST in every module found in the module map.
    pub fn validate(&mut self, module_map: ModuleMap) -> Result<()> {
        for module in module_map.modules {
            debug!("AST validating module: {:?}", module.barrel.file_id);
            for mut item in module.barrel.items {
                self.visit_item(&mut item);
            }
        }

        Ok(())
    }
}

impl<'a> AstWalker for AstValidator<'a> {
    fn visit_item(&mut self, item: &mut Item) {
        todo!()
    }
}
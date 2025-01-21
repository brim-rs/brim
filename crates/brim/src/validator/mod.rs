use crate::resolver::Resolver;
use anyhow::Result;
use brim_ctx::compiler::CompilerContext;
use brim_ctx::modules::ModuleMap;

#[derive(Debug)]
pub struct AstValidator<'a> {
    pub ctx: &'a mut CompilerContext<'a>,
}

impl<'a> AstValidator<'a> {
    pub fn new(ctx: &'a mut CompilerContext<'a>) -> Self {
        Self { ctx }
    }

    pub fn validate(&mut self) -> Result<()> {
        Ok(())
    }
}
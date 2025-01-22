mod errors;

use crate::resolver::Resolver;
use anyhow::Result;
use tracing::debug;
use brim_ast::item::{Item, ItemKind};
use brim_ctx::compiler::CompilerContext;
use brim_ctx::modules::ModuleMap;
use brim_ctx::walker::AstWalker;
use crate::validator::errors::TooManyParameters;

#[derive(Debug)]
pub struct AstValidator<'a> {
    pub ctx: &'a mut CompilerContext<'a>,
    pub current_file: usize
}

impl<'a> AstValidator<'a> {
    pub fn new(ctx: &'a mut CompilerContext<'a>) -> Self {
        Self { ctx, current_file: 0 }
    }

    /// Validates AST in every module found in the module map.
    pub fn validate(&mut self, module_map: ModuleMap) -> Result<()> {
        for module in module_map.modules {
            debug!("AST validating module: {:?}", module.barrel.file_id);
            
            self.current_file = module.barrel.file_id.clone();
            for mut item in module.barrel.items {
                self.visit_item(&mut item);
            }
        }

        Ok(())
    }
}

impl<'a> AstWalker for AstValidator<'a> {
    fn visit_item(&mut self, item: &mut Item) {
        match &item.kind {
            // Checking for empty body and self param is already handled by the parser
            ItemKind::Fn(func) => {
                if func.sig.params.len() > 255 {
                    self.ctx.emit(TooManyParameters {
                        span:(func.sig.span.clone(), self.current_file),
                        note: "Because we compile to C++, we have to limit the number of parameters to 255."
                    });
                }
            }
            _ => {}
        }
    }
}
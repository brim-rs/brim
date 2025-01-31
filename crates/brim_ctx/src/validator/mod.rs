mod errors;

use crate::validator::errors::{DuplicateParam, TooManyParameters};
use anyhow::Result;
use brim_ast::item::{FnSignature, Item, ItemKind};
use brim_middle::{modules::ModuleMap, walker::AstWalker};
use brim_span::span::Span;
use std::collections::HashMap;
use tracing::debug;
use brim_middle::temp_diag::TemporaryDiagnosticContext;

#[derive(Debug)]
pub struct AstValidator {
    pub ctx: TemporaryDiagnosticContext,
    pub current_file: usize,
}

impl AstValidator {
    pub fn new() -> Self {
        Self {
            ctx: TemporaryDiagnosticContext::new(),
            current_file: 0,
        }
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

    pub fn validate_function_params(&mut self, sig: &FnSignature) {
        if sig.params.len() > 255 {
            self.ctx.emit(Box::new(TooManyParameters {
                span: (sig.span.clone(), self.current_file),
                note: "Because we compile to C++, we have to limit the number of parameters to 255.",
            }));
        }

        let mut seen: HashMap<String, Span> = HashMap::new();
        for param in &sig.params {
            let param_name = param.name.to_string();
            if let Some(original_span) = seen.get(&param_name) {
                self.ctx.emit(Box::new(DuplicateParam {
                    dup: (param.span.clone(), self.current_file),
                    span: (original_span.clone(), self.current_file),
                    name: param_name,
                }));
            } else {
                seen.insert(param_name, param.span.clone());
            }
        }
    }
}

impl AstWalker for AstValidator {
    fn visit_item(&mut self, item: &mut Item) {
        match &mut item.kind {
            ItemKind::Fn(func) => {
                self.validate_function_params(&func.sig);

                if let Some(ref mut block) = func.body {
                    self.walk_block(block);
                }
            }
            _ => {}
        }
    }
}

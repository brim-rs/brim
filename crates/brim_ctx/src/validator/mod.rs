mod errors;

use crate::validator::errors::{
    BuiltinFunctionArgCount, DuplicateFieldInitializer, DuplicateParam, TooManyParameters,
};
use anyhow::Result;
use brim_ast::{
    expr::{Expr, ExprKind},
    item::{FnSignature, GenericArgs, Ident, Item, ItemKind},
};
use brim_middle::{
    builtins::BUILTIN_FUNCTIONS, modules::ModuleMap, temp_diag::TemporaryDiagnosticContext,
    walker::AstWalker,
};
use brim_span::span::Span;
use indexmap::IndexMap;
use std::collections::HashMap;
use tracing::debug;

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
            ItemKind::External(external) => {
                for item in &mut external.items {
                    self.visit_item(item);
                }
            }
            ItemKind::Struct(str) => {
                for item in str.items.iter_mut() {
                    self.visit_item(item);
                }
            }
            ItemKind::Namespace(_)
            | ItemKind::Module(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::Use(_) => {}
            _ => todo!("missing implementation for {:?}", item.kind),
        }
    }

    fn visit_struct_constructor(
        &mut self,
        _ident: &mut Ident,
        _: &mut GenericArgs,
        fields: &mut IndexMap<Ident, Expr>,
    ) {
        let mut seen: HashMap<String, Span> = HashMap::new();

        for (ident, _) in fields.iter() {
            let ident_name = ident.to_string();
            if let Some(original_span) = seen.get(&ident_name) {
                self.ctx.emit(Box::new(DuplicateFieldInitializer {
                    second: (original_span.clone(), self.current_file),
                    first: (ident.span.clone(), self.current_file),
                    name: ident_name,
                }));
            } else {
                seen.insert(ident_name, ident.span.clone());
            }
        }
    }

    fn visit_builtin(&mut self, ident: &mut Ident, args: &mut Vec<Expr>) {
        let name = ident.to_string();
        let func = BUILTIN_FUNCTIONS.get(&name);

        if let Some(func) = func {
            if args.len() != func.expected_args {
                self.ctx.emit_impl(BuiltinFunctionArgCount {
                    span: (ident.span.clone(), self.current_file),
                    expected: func.expected_args,
                    found: args.len(),
                });
            }
        }
    }
}

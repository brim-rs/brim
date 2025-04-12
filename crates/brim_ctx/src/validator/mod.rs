mod errors;

use crate::validator::errors::{
    BuiltinFunctionArgCount, DuplicateFieldInitializer, DuplicateParam, ExternFunctionResultOption,
    TooManyParameters,
};
use anyhow::Result;
use brim_ast::{
    expr::Expr,
    item::{FnSignature, GenericArgs, Ident, Item, ItemKind},
};
use brim_middle::{
    builtins::BUILTIN_FUNCTIONS, modules::ModuleMap, temp_diag::TemporaryDiagnosticContext,
    walker::AstWalker,
};
use brim_span::span::Span;
use errors::DuplicateVariantName;
use indexmap::IndexMap;
use std::collections::HashMap;
use tracing::debug;

#[derive(Debug)]
pub struct AstValidator {
    pub ctx: TemporaryDiagnosticContext,
    pub current_file: usize,
    pub external: bool,
}

impl Default for AstValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl AstValidator {
    pub fn new() -> Self {
        Self { ctx: TemporaryDiagnosticContext::new(), current_file: 0, external: false }
    }

    /// Validates AST in every module found in the module map.
    pub fn validate(&mut self, module_map: ModuleMap) -> Result<()> {
        for module in module_map.modules {
            debug!("AST validating module: {:?}", module.barrel.file_id);

            self.current_file = module.barrel.file_id;
            for mut item in module.barrel.items {
                self.visit_item(&mut item);
            }
        }

        Ok(())
    }

    pub fn validate_function_params(&mut self, sig: &FnSignature) {
        if sig.params.len() > 255 {
            self.ctx.emit(Box::new(TooManyParameters {
                span: (sig.span, self.current_file),
                note: "Because we compile to C++, we have to limit the number of parameters to 255.",
            }));
        }

        let mut seen: HashMap<String, Span> = HashMap::new();
        for param in &sig.params {
            let param_name = param.name.to_string();
            if let Some(original_span) = seen.get(&param_name) {
                self.ctx.emit_impl(DuplicateParam {
                    dup: (param.span, self.current_file),
                    span: (*original_span, self.current_file),
                    name: param_name,
                });
            } else {
                seen.insert(param_name, param.span);
            }

            if !param.ty.kind.allowed_in_external() && self.external {
                self.ctx.emit_impl(ExternFunctionResultOption {
                    span: (param.span, self.current_file),
                });
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
                self.external = true;
                for item in &mut external.items {
                    self.visit_item(item);
                }
                self.external = false;
            }
            ItemKind::Struct(str) => {
                for item in &mut str.items {
                    self.visit_item(item);
                }
            }
            ItemKind::Enum(e) => {
                for item in &mut e.items {
                    self.visit_item(item);
                }

                for variant in e.variants.clone() {
                    // we check if there is an item with the same name as the variant
                    if let Some(item) = e.find_item(&variant.ident) {
                        self.ctx.emit_impl(DuplicateVariantName {
                            dup: (item.span, self.current_file),
                            span: (variant.span, self.current_file),
                            name: variant.ident.to_string(),
                        });
                    }
                }
            }
            ItemKind::Namespace(_)
            | ItemKind::Module(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::Use(_) => {}
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
                    second: (*original_span, self.current_file),
                    first: (ident.span, self.current_file),
                    name: ident_name,
                }));
            } else {
                seen.insert(ident_name, ident.span);
            }
        }
    }

    fn visit_builtin(&mut self, ident: &mut Ident, args: &mut Vec<Expr>) {
        let name = ident.to_string();
        let func = BUILTIN_FUNCTIONS.get(&name);

        if let Some(func) = func {
            if args.len() != func.expected_args {
                self.ctx.emit_impl(BuiltinFunctionArgCount {
                    span: (ident.span, self.current_file),
                    expected: func.expected_args,
                    found: args.len(),
                });
            }
        }
    }
}

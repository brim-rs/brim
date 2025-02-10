use crate::{
    diag_ctx::DiagnosticContext,
    errors::{MainFunctionConstant, MainFunctionParams, SymbolNotFound, SymbolPrivate},
    name::NameResolver,
    validator::AstValidator,
};
use anyhow::Result;
use brim_ast::item::{ImportsKind, ItemKind, Visibility, VisibilityKind};
use brim_codegen::codegen::CppCodegen;
use brim_diagnostics::{
    ErrorEmitted,
    diagnostic::{Diagnostic, ToDiagnostic},
};
use brim_hir::{
    Codegen,
    builtin::expand_builtins,
    inference::infer_types,
    items::HirFn,
    transformer::{HirModuleMap, transform_module},
    type_checker::TypeChecker,
};
use brim_middle::{
    GlobalSymbolId, ModuleId,
    args::RunArgs,
    lints::Lints,
    modules::{ModuleMap, SymbolCollector},
    temp_diag::TemporaryDiagnosticContext,
};
use brim_span::files::{SimpleFiles, files, get_file};

#[derive(Debug, Clone)]
pub struct CompilerContext {
    dcx: DiagnosticContext,
    pub emitted: Vec<Diagnostic<usize>>,
    pub args: RunArgs,
    pub lints: &'static Lints,
}

impl CompilerContext {
    pub fn new(args: RunArgs, lints: &'static Lints) -> Self {
        Self {
            dcx: DiagnosticContext::new(),
            emitted: vec![],
            args,
            lints,
        }
    }

    pub fn dcx(&mut self) -> &mut DiagnosticContext {
        &mut self.dcx
    }

    pub fn emit(&mut self, diag: impl ToDiagnostic + 'static) -> ErrorEmitted {
        self.emit_inner(Box::new(diag))
    }

    pub fn emit_diag(&mut self, diag: Diagnostic<usize>) -> ErrorEmitted {
        self.dcx
            .emit_inner(diag.clone(), &SimpleFiles::from_files(files()));
        self.emitted.push(diag);

        ErrorEmitted::new()
    }

    pub fn emit_inner(&mut self, diag: Box<dyn ToDiagnostic>) -> ErrorEmitted {
        let diag_clone = diag.to_diagnostic();
        self.dcx
            .emit(&Box::new(diag), &SimpleFiles::from_files(files()));
        self.emitted.push(diag_clone);

        ErrorEmitted::new()
    }

    pub fn extend_temp(&mut self, temp: TemporaryDiagnosticContext) {
        for diag in temp.diags.iter() {
            self.emit_diag(diag.clone());
        }
    }

    /// Resolve and analyze the project form the main barrel
    pub fn analyze(&mut self, mut map: ModuleMap) -> Result<HirModuleMap> {
        let map = &mut map;

        let mut validator = AstValidator::new();
        validator.validate(map.clone())?;
        self.extend_temp(validator.ctx);

        let mut collector = SymbolCollector::new(map);
        collector.collect();

        for module in map.modules.clone() {
            let file_id = module.barrel.file_id;
            for item in module.barrel.items {
                match item.kind {
                    ItemKind::Use(u) => {
                        let module_id = ModuleId::from_usize(file_id);
                        let module = map
                            .module_by_import(GlobalSymbolId {
                                mod_id: module_id,
                                item_id: item.id,
                            })
                            .unwrap();
                        let resolved_id = ModuleId::from_usize(module.barrel.file_id);

                        let import_symbols: Vec<GlobalSymbolId> = match &u.imports {
                            ImportsKind::All => map
                                .find_symbols_in_module(Some(resolved_id))
                                .iter()
                                .map(|symbol| GlobalSymbolId {
                                    mod_id: resolved_id,
                                    item_id: symbol.item_id,
                                })
                                .collect(),
                            ImportsKind::List(list) => list
                                .iter()
                                .map(|import| {
                                    (
                                        map.find_symbol_by_name(
                                            &import.to_string(),
                                            Some(resolved_id),
                                        ),
                                        import,
                                    )
                                })
                                .filter_map(|(symbol, import)| {
                                    let mod_path = get_file(resolved_id.as_usize())
                                        .unwrap()
                                        .name()
                                        .display()
                                        .to_string();
                                    if let Some(s) = symbol {
                                        if s.vis.kind == VisibilityKind::Private {
                                            self.emit(SymbolPrivate {
                                                imported: (import.span, file_id),
                                                defined: (s.span(), resolved_id.as_usize()),
                                                name: import.to_string(),
                                                module: mod_path,
                                                note: "mark this item as public by adding `pub` at the beginning",
                                            });
                                        }

                                        Some(GlobalSymbolId {
                                            mod_id: resolved_id,
                                            item_id: s.item_id,
                                        })
                                    } else {
                                        self.emit(SymbolNotFound {
                                            span: (import.span, file_id),
                                            name: import.to_string(),
                                            module: mod_path,
                                        });

                                        None
                                    }
                                })
                                .collect(),
                        };

                        map.update_modules_imports(module_id, import_symbols);
                    }
                    _ => {}
                }
            }
        }

        let mut name_resolver = NameResolver::new(map.clone(), self.lints);
        name_resolver.resolve_names();
        self.extend_temp(name_resolver.ctx);

        let (hir, hir_temp) = &mut transform_module(name_resolver.map);
        self.extend_temp(hir_temp.clone());
        let ti = infer_types(hir);
        self.extend_temp(ti.temp.clone());

        if ti.temp.diags.is_empty() {
            expand_builtins(hir);
            let mut type_analyzer = TypeChecker::new(hir.clone());
            type_analyzer.check();
            self.extend_temp(type_analyzer.ctx);

            Ok(type_analyzer.hir.clone())
        } else {
            Ok(hir.clone())
        }
    }

    pub fn run_codegen(&mut self, hir: HirModuleMap) -> String {
        let mut codegen = CppCodegen::new(hir.clone());
        codegen.generate();

        let code = codegen.code.build();
        if self.args.codegen_debug {
            println!("{}", code.clone());
        }

        code
    }

    /// More to be added
    pub fn validate_main_function(&mut self, func: &HirFn, main: usize) {
        if func.sig.params.params.len() != 0 {
            self.emit(MainFunctionParams {
                span: (func.sig.params.span, main),
            });
        }

        if func.sig.constant {
            self.emit(MainFunctionConstant {
                span: (func.sig.span, main),
            });
        }
    }
}

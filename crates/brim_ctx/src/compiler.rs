use crate::{
    diag_ctx::DiagnosticContext,
    errors::{MainFunctionConstant, MainFunctionParams},
    name::NameResolver,
    validator::AstValidator,
};
use anyhow::Result;
use brim_diagnostics::{
    ErrorEmitted,
    diagnostic::{Diagnostic, ToDiagnostic},
};
use brim_hir::{
    CompiledModules,
    comptime::transform_comptime,
    inference::infer_types,
    items::{HirFn, HirItem, HirItemKind},
    transformer::{HirModuleMap, transform_module},
    type_checker::TypeChecker,
};
use brim_middle::{
    ModuleId,
    args::RunArgs,
    lints::Lints,
    modules::{ModuleMap, SymbolCollector, UseCollector},
    temp_diag::TemporaryDiagnosticContext,
};
use brim_span::{
    files::{SimpleFiles, files},
    span::Span,
};

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

    pub fn analyze(
        &mut self,
        mut map: ModuleMap,
        compiled: &mut CompiledModules,
    ) -> Result<HirModuleMap> {
        let map = &mut map;

        let mut validator = AstValidator::new();
        validator.validate(map.clone())?;
        self.extend_temp(validator.ctx);

        let mut collector = SymbolCollector::new(&mut compiled.symbols);
        collector.collect(map);

        let mut use_collector = UseCollector::new(&mut compiled.symbols);
        use_collector.collect(map);

        for ((ident, id), symbols) in &use_collector.namespaces {
            compiled.items.insert(id.clone(), HirItem {
                id: id.clone(),
                span: Span::DUMMY,
                ident: ident.clone(),
                kind: HirItemKind::Namespace(symbols.clone()),
                is_public: true,
                mod_id: ModuleId::new(),
            });
        }

        let mut name_resolver = NameResolver::new(map.clone(), self.lints, compiled);
        name_resolver.resolve_names();
        self.extend_temp(name_resolver.ctx);

        let (hir, hir_temp) = &mut transform_module(name_resolver.map, compiled);
        self.extend_temp(hir_temp.clone());

        let comp_transformer = transform_comptime(hir, compiled);
        self.extend_temp(comp_transformer.temp.clone());

        let ti = infer_types(hir, compiled);
        self.extend_temp(ti.temp.clone());

        if ti.temp.diags.is_empty() {
            let mut type_analyzer = TypeChecker::new(hir.clone(), compiled.clone());
            type_analyzer.check();
            self.extend_temp(type_analyzer.ctx);

            Ok(type_analyzer.hir.clone())
        } else {
            Ok(hir.clone())
        }
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

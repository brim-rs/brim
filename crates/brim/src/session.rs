use crate::{name::NameResolver, validator::AstValidator};
use anstream::ColorChoice;
use anyhow::{Result, bail};
use brim_ast::item::{ImportsKind, ItemKind};
use brim_codegen::codegen::CppCodegen;
use brim_config::toml::{Config, LibType, ProjectType};
use brim_ctx::{GlobalSymbolId, ModuleId, compiler::CompilerContext};
use brim_diag_macro::Diagnostic;
use brim_diagnostics::{
    box_diag,
    diagnostic::{Label, LabelStyle, Severity, ToDiagnostic},
};
use brim_fs::{
    loader::{BrimFileLoader, FileLoader},
    path,
};
use brim_hir::{
    inference::infer_types,
    items::HirFn,
    transformer::{HirModuleMap, transform_module},
    ty::HirTyKind,
};
use brim_middle::modules::{ModuleMap, SymbolCollector};
use brim_parser::parser::PResult;
use brim_shell::Shell;
use brim_span::{
    files::{SimpleFile, add_file, get_file, get_file_by_name, get_index_by_name, update_file},
    span::Span,
};
use std::{path::PathBuf, time::Instant};
use tracing::debug;

#[derive(Debug)]
pub struct Session {
    pub config: Config,
    cwd: PathBuf,
    pub color_choice: ColorChoice,
    start: Instant,
    pub measure_time: bool,
    file_loader: BrimFileLoader,
    pub shell: Shell,
    pub display_cpp: bool,
    pub no_write: bool,
}

impl Session {
    pub fn new(cwd: PathBuf, config: Config, color_choice: ColorChoice) -> Self {
        Self {
            config,
            cwd,
            color_choice,
            start: Instant::now(),
            measure_time: false,
            file_loader: BrimFileLoader,
            shell: Shell::new(color_choice),
            display_cpp: false,
            no_write: false,
        }
    }

    pub fn add_file(&mut self, name: PathBuf, source: String) -> usize {
        if let Ok(file) = get_index_by_name(&name) {
            update_file(file, name, source);

            return file;
        }

        add_file(name, source)
    }

    pub fn get_file(&self, file: usize) -> Option<SimpleFile> {
        get_file(file).ok()
    }

    pub fn get_file_by_name(&self, name: &PathBuf) -> Option<SimpleFile> {
        get_file_by_name(name).ok()
    }

    pub fn shell(&mut self) -> &mut Shell {
        &mut self.shell
    }

    /// Assert that the current project is of a certain type
    pub fn assert_type(&self, typ: ProjectType, message: impl Into<String>) -> Result<()> {
        debug!("asserting project type: {:?}", typ);

        if self.config.project.r#type != typ {
            bail!("{}", message.into());
        }

        Ok(())
    }

    pub fn project_type(&self) -> ProjectType {
        self.config.project.r#type.clone()
    }

    pub fn lib_type(&self) -> LibType {
        self.config.build.lib_type.clone()
    }

    /// Measure the time taken to run a closure and print it to the shell
    pub fn measure_time(
        &mut self,
        f: impl FnOnce(&mut Session) -> Result<()>,
        msg: impl Into<String>,
    ) -> Result<()> {
        let start = Instant::now();
        f(self)?;

        if self.measure_time {
            let elapsed = start.elapsed();
            self.shell()
                .status("Took", format!("{:?} {}", elapsed, msg.into()))?;
        }

        Ok(())
    }

    pub fn main_file(&mut self) -> Result<usize> {
        let file = if self.config.is_bin() {
            self.config.binary_path(path(vec!["src", "main.brim"]))
        } else {
            self.config.library_path(path(vec!["src", "lib.brim"]))
        };

        let path = self.cwd.join(&file);
        self.file_loader.check_if_exists(&path)?;

        debug!("main file: {:?}", path);
        Ok(self.add_file(path.clone(), self.file_loader.read_file(&path)?))
    }

    pub fn build_dir(&self) -> PathBuf {
        self.cwd.join("build")
    }

    /// Resolve and analyze the project form the main barrel
    pub fn analyze<'a>(
        &mut self,
        mut map: ModuleMap,
        ctx: &'a mut CompilerContext<'a>,
    ) -> Result<(HirModuleMap, &'a mut CompilerContext<'a>)> {
        let map = &mut map;

        let mut validator = AstValidator::new(ctx);
        validator.validate(map.clone())?;

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
                                .flat_map(|import| {
                                    map.find_symbol_by_name(&import.to_string(), Some(resolved_id))
                                })
                                .map(|symbol| GlobalSymbolId {
                                    mod_id: resolved_id,
                                    item_id: symbol.item_id,
                                })
                                .collect(),
                        };

                        map.update_modules_imports(module_id, import_symbols);
                    }
                    _ => {}
                }
            }
        }

        let mut name_resolver = NameResolver::new(validator.ctx, map.clone());
        name_resolver.resolve_names();

        let hir = &mut transform_module(name_resolver.map);
        infer_types(hir);

        // TODO: type checking etc.

        Ok((hir.clone(), name_resolver.ctx))
    }

    pub fn run_codegen(&mut self, hir: HirModuleMap) -> String {
        let mut codegen = CppCodegen::new(hir.clone());
        codegen.generate();

        let code = codegen.code.build();
        if self.display_cpp {
            println!("{}", code.clone());
        }

        code
    }

    /// More to be added
    pub fn validate_main_function(
        &mut self,
        func: &HirFn,
        comp: &mut CompilerContext,
        main: usize,
    ) {
        if func.sig.params.params.len() != 0 {
            comp.emit(MainFunctionParams {
                span: (func.sig.params.span, main),
            });
        }

        if func.sig.constant {
            comp.emit(MainFunctionConstant {
                span: (func.sig.span, main),
            });
        }
    }
}

#[derive(Diagnostic)]
#[error("entrypoint file: '{file}' doesn't contain a main function")]
pub struct NoMainFunction {
    pub file: String,
}

#[derive(Diagnostic)]
#[error("main function cannot have parameters")]
pub struct MainFunctionParams {
    #[error("delete the parameters")]
    pub span: (Span, usize),
}

#[derive(Diagnostic)]
#[error("main function cannot be constant")]
pub struct MainFunctionConstant {
    #[error("delete the `const` keyword")]
    pub span: (Span, usize),
}

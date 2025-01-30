use anstream::ColorChoice;
use anyhow::{Result, bail};
use brim_ast::item::{ImportsKind, ItemKind};
use brim_codegen::codegen::CppCodegen;
use brim_config::toml::{Config, LibType, ProjectType};
use brim_ctx::{compiler::CompilerContext, name::NameResolver, validator::AstValidator};
use brim_diag_macro::Diagnostic;
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
}

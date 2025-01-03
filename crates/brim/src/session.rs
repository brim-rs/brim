use crate::{compiler::CompilerContext, diag_ctx::DiagnosticContext};
use anstream::ColorChoice;
use anyhow::{Result, bail};
use brim_config::toml::{Config, ProjectType};
use brim_fs::{
    loader::{BrimFileLoader, FileLoader},
    path,
};
use brim_shell::Shell;
use brim_span::files::{SimpleFile, SimpleFiles};
use std::{path::PathBuf, time::Instant};
use tracing::debug;
use brim_diagnostics::diagnostic::ToDiagnostic;

#[derive(Debug)]
pub struct Session<'a> {
    files: SimpleFiles,
    config: Config,
    cwd: PathBuf,
    color_choice: ColorChoice,
    dcx: DiagnosticContext,
    start: Instant,
    pub measure_time: bool,
    file_loader: BrimFileLoader,
    compiler: &'a CompilerContext,
    shell: Shell,
}

impl<'a> Session<'a> {
    pub fn new(
        cwd: PathBuf,
        config: Config,
        color_choice: ColorChoice,
        comp: &'a CompilerContext,
    ) -> Self {
        Self {
            files: SimpleFiles::new(),
            config,
            cwd,
            color_choice,
            start: Instant::now(),
            measure_time: false,
            file_loader: BrimFileLoader,
            shell: Shell::new(color_choice),
            dcx: DiagnosticContext::new(),
            compiler: comp,
        }
    }

    pub fn add_file(&mut self, name: PathBuf, source: String) -> usize {
        if let Ok(file) = self.files.get_index_by_name(&name) {
            self.files.update(file, name, source);

            return file;
        }

        self.files.add(name, source)
    }

    pub fn get_file(&self, file: usize) -> Option<SimpleFile> {
        self.files.get(file).ok().cloned()
    }

    pub fn get_file_by_name(&self, name: &PathBuf) -> Option<&SimpleFile> {
        self.files.get_by_name(name).ok()
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

    pub fn dcx(&mut self) -> &mut DiagnosticContext {
        &mut self.dcx
    }

    pub fn emit(&mut self, diag: impl ToDiagnostic<'a>) {
        self.dcx.emit(diag, &self.files);
    }
}

use std::path::PathBuf;
use std::time::Instant;
use anstream::ColorChoice;
use anyhow::bail;
use brim_config::toml::{Config, ProjectType};
use brim_shell::Shell;
use brim_span::files::{SimpleFile, SimpleFiles};
use anyhow::Result;
use tracing::debug;
use brim_fs::loader::{BrimFileLoader, FileLoader};
use brim_fs::path;
use brim_span::file::FileId;
use crate::diag_ctx::DiagnosticContext;

#[derive(Debug)]
pub struct Session<'a> {
    files: SimpleFiles,
    config: Config,
    cwd: PathBuf,
    shell: Shell,
    color_choice: ColorChoice,
    dcx: DiagnosticContext<'a>,
    pub start: Instant,
    pub measure_time: bool,
    pub file_loader: BrimFileLoader,
}

impl<'a> Session<'a> {
    pub fn new(cwd: PathBuf, config: Config, color_choice: ColorChoice) -> Self {
        Self {
            files: SimpleFiles::new(),
            config,
            cwd,
            color_choice,
            shell: Shell::new(color_choice),
            start: Instant::now(),
            measure_time: false,
            file_loader: BrimFileLoader,
            dcx: DiagnosticContext::new()
        }
    }

    pub fn add_file(&mut self, name: PathBuf, source: String) -> usize {
        if let Ok(file) = self.files.get_index_by_name(&name) {
            self.files.update(file, name, source);

            return file;
        }

        self.files.add(name, source)
    }

    pub fn get_file(&self, file: usize) -> Option<&SimpleFile> {
        self.files.get(file).ok()
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
    pub fn measure_time(&mut self, f: impl FnOnce(
        &mut Session,
    ) -> Result<()>, msg: impl Into<String>) -> Result<()> {
        let start = Instant::now();
        f(self)?;

        if self.measure_time {
            let elapsed = start.elapsed();
            self.shell().status("Took", format!("{:?} {}", elapsed, msg.into()))?;
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

    pub fn dcx(&mut self) -> &mut DiagnosticContext<'a> {
        &mut self.dcx
    }
}
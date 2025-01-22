use anstream::ColorChoice;
use anyhow::{Result, bail};
use brim_config::toml::{Config, ProjectType};
use brim_ctx::{barrel::Barrel};
use brim_fs::{
    loader::{BrimFileLoader, FileLoader},
    path,
};
use brim_shell::Shell;
use brim_span::files::{
    SimpleFile, add_file, get_file, get_file_by_name, get_index_by_name, update_file,
};
use std::{path::PathBuf, time::Instant};
use tracing::debug;
use brim_ctx::compiler::CompilerContext;
use brim_ctx::modules::{ModuleMap, SymbolCollector};
use crate::resolver::Resolver;
use crate::validator::AstValidator;

#[derive(Debug)]
pub struct Session {
    config: Config,
    cwd: PathBuf,
    color_choice: ColorChoice,
    start: Instant,
    pub measure_time: bool,
    file_loader: BrimFileLoader,
    shell: Shell,
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

    /// Resolve and analyze the project form the main barrel
    pub fn analyze<'a>(
        &mut self,
        map: &mut ModuleMap,
        ctx: &'a mut CompilerContext<'a>,
    ) -> Result<()> {
        let validator = &mut AstValidator::new(ctx);
        validator.validate(map.clone())?;

        SymbolCollector::new(map).collect();

        Ok(())
    }
}


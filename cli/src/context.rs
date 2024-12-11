use anstream::ColorChoice;
use anyhow::{anyhow, bail, Context, Result};
use std::{fs::read_to_string, path::PathBuf, sync::Arc, time::Instant};
use brim_shell::Shell;
use crate::error::diagnostic::{Diagnostic, Level};
use crate::error::span::TextSpan;
use crate::lexer::source::Source;

#[derive(Debug)]
pub struct GlobalContext {
    pub verbose: bool,
    pub cwd: PathBuf,
    pub start: Instant,
    pub shell: Shell,
    pub diagnostics: Vec<(Diagnostic, Option<Arc<Source>>)>,
}

impl GlobalContext {
    pub fn default(color_choice: ColorChoice) -> Result<Self> {
        Ok(Self {
            verbose: false,
            cwd: std::env::current_dir().context("Failed to get current directory")?,
            start: Instant::now(),
            shell: Shell::new(color_choice),
            diagnostics: Vec::new(),
        })
    }

    pub fn from_cwd(cwd: PathBuf, color_choice: ColorChoice) -> Result<Self> {
        Ok(Self {
            verbose: false,
            cwd,
            start: Instant::now(),
            shell: Shell::new(color_choice),
            diagnostics: Vec::new(),
        })
    }

    pub fn build_dir(&self) -> Result<PathBuf> {
        Ok(self.cwd.join("build"))
    }

    pub fn deps_dir(&self) -> Result<PathBuf> {
        Ok(self.build_dir()?.join("deps"))
    }

    pub fn cache_dir(&self) -> Result<PathBuf> {
        Ok(dirs::cache_dir().unwrap().join("brim"))
    }

    pub fn warning(&mut self, message: String, source: Option<Arc<Source>>, span: Option<TextSpan>, hint: Option<String>) {
        self.diagnostics.push((Diagnostic {
            text: message,
            level: Level::Warning,
            span,
            hint,
        }, source));
    }
    
    pub fn new_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push((diagnostic, None));
    }

    pub fn print_diagnostics(&mut self) {
        for (diagnostic, source) in self.diagnostics.clone() {
            diagnostic.write(&mut self.shell, source).unwrap();
        }
    }
}

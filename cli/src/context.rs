use anstream::ColorChoice;
use anyhow::{anyhow, bail, Context, Result};
use std::{fs::read_to_string, path::PathBuf, sync::Arc, time::Instant};
use brim_shell::Shell;

#[derive(Debug)]
pub struct GlobalContext {
    pub verbose: bool,
    pub cwd: PathBuf,
    pub start: Instant,
    pub shell: Shell,
}

impl GlobalContext {
    pub fn default(color_choice: ColorChoice) -> Result<Self> {
        Ok(Self {
            verbose: false,
            cwd: std::env::current_dir().context("Failed to get current directory")?,
            start: Instant::now(),
            shell: Shell::new(color_choice),
        })
    }

    pub fn from_cwd(cwd: PathBuf, color_choice: ColorChoice) -> Result<Self> {
        Ok(Self {
            verbose: false,
            cwd,
            start: Instant::now(),
            shell: Shell::new(color_choice),
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
}

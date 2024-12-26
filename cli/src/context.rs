use crate::{
    path::{canonicalize_path, normalize_without_canonicalize},
};
use anstream::ColorChoice;
use anyhow::{anyhow, Context, Result};
use brim_shell::Shell;
use colored::Colorize;
use std::{fs::read_to_string, path::PathBuf, time::Instant};
use std::fmt::{Debug, Display};
use clap::ArgMatches;
use brim_config::{BrimConfig, ProjectType};
use brim_config::parsed::ParsedBrimConfig;

pub struct GlobalContext {
    pub verbose: bool,
    pub cwd: PathBuf,
    pub start: Instant,
    pub config: ParsedBrimConfig,
}

impl GlobalContext {
    pub fn load_config(cwd: PathBuf, args: Option<&ArgMatches>) -> Result<ParsedBrimConfig> {
        Ok(ParsedBrimConfig::get(cwd, args)?)
    }

    pub fn default(color_choice: ColorChoice, args: Option<&ArgMatches>) -> Result<Self> {
        let cwd = std::env::current_dir().context("Failed to get current directory")?;
        let config = GlobalContext::load_config(cwd.clone(), args)?;

        Ok(Self {
            verbose: false,
            cwd,
            start: Instant::now(),
            config,
        })
    }

    pub fn project_type(&self) -> Result<ProjectType> {
        Ok(self.config.project.r#type.clone().unwrap())
    }

    pub fn is_lib(&self) -> Result<bool> {
        Ok(self.project_type()? == ProjectType::Lib)
    }

    pub fn is_bin(&self) -> Result<bool> {
        Ok(self.project_type()? == ProjectType::Bin)
    }

    pub fn get_main_file(&self) -> Result<PathBuf> {
        let file: PathBuf = match self.project_type()? {
            ProjectType::Lib => self
                .config
                .project
                .lib
                .clone()
                .unwrap_or_else(|| self.cwd.join("src\\lib.brim")),
            ProjectType::Bin => self
                .config
                .project
                .bin
                .clone()
                .unwrap_or_else(|| self.cwd.join("src\\main.brim")),
        };

        let path = normalize_without_canonicalize(file, self.cwd.clone());
        if !path.exists() {
            return Err(anyhow!("Main file does not exist: {}", path.display()));
        }

        Ok(canonicalize_path(path)?)
    }

    pub fn get_main_dir(&self) -> Result<PathBuf> {
        Ok(self.get_main_file()?.parent().unwrap().to_path_buf())
    }

    pub fn from_cwd(cwd: PathBuf) -> Result<Self> {
        let config = GlobalContext::load_config(cwd.clone(), None)?;

        Ok(Self {
            verbose: false,
            cwd,
            start: Instant::now(),
            config,
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

impl Debug for GlobalContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GlobalContext")
            .field("verbose", &self.verbose)
            .field("cwd", &self.cwd)
            .field("start", &self.start)
            .field("config", &self.config)
            .finish()
    }
}
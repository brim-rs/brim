use crate::{
    fs::walk_for_file,
    path::{canonicalize_path, normalize_without_canonicalize},
};
use anstream::ColorChoice;
use anyhow::{anyhow, Context, Result};
use brim_shell::Shell;
use colored::Colorize;
use std::{fs::read_to_string, path::PathBuf, time::Instant};
use std::fmt::{Debug, Display};
use brim_config::BrimConfig;

pub struct GlobalContext {
    pub verbose: bool,
    pub cwd: PathBuf,
    pub start: Instant,
    pub shell: Shell,
    pub config: BrimConfig,
}

#[derive(Debug, PartialEq)]
pub enum ProjectType {
    Lib,
    Bin,
}

impl GlobalContext {
    pub fn load_config(cwd: PathBuf) -> Result<BrimConfig> {
        let path = walk_for_file(cwd.clone(), "brim.toml");

        if path.is_none() {
            return Err(anyhow!("Failed to find {}. Make sure you are running the command inside project root or in a subdirectory", "brim.toml".bright_magenta()));
        }

        let path = path.unwrap();

        let content = read_to_string(&path).context("Failed to read roan.toml")?;
        let config: BrimConfig = toml::from_str(&content)?;

        if config.project.r#type.is_none() {
            return Err(anyhow!(
                "Project type is not specified in [project] in roan.toml. Available types: 'lib', 'bin'"
            ));
        }

        let r#type = config.project.r#type.as_ref().unwrap();
        if r#type != "lib" && r#type != "bin" {
            return Err(anyhow!(
                "Invalid project type in [project] in roan.toml. Available types: 'lib', 'bin'"
            ));
        }

        Ok(config)
    }

    pub fn default(color_choice: ColorChoice) -> Result<Self> {
        let cwd = std::env::current_dir().context("Failed to get current directory")?;
        let config = GlobalContext::load_config(cwd.clone())?;

        Ok(Self {
            verbose: false,
            cwd,
            start: Instant::now(),
            shell: Shell::new(color_choice),
            config,
        })
    }

    pub fn project_type(&self) -> Result<ProjectType> {
        match self.config.project.r#type.as_ref().unwrap().as_str() {
            "lib" => Ok(ProjectType::Lib),
            "bin" => Ok(ProjectType::Bin),
            _ => unreachable!(),
        }
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

    pub fn from_cwd(cwd: PathBuf, color_choice: ColorChoice) -> Result<Self> {
        let config = GlobalContext::load_config(cwd.clone())?;

        Ok(Self {
            verbose: false,
            cwd,
            start: Instant::now(),
            shell: Shell::new(color_choice),
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
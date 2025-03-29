use crate::errors::ConfigError;
use anyhow::{Context, Result, ensure};
use brim_fs::{canonicalize_path, walk_dir::walk_for_file};
use brim_middle::{experimental::Experimental, lints::LintsConfig};
use clap::ArgMatches;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fmt::Display, fs::read_to_string, path::PathBuf};
use tracing::debug;

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct BrimConfig {
    pub project: ProjectConfig,
    pub tasks: Option<HashMap<String, String>>,
    pub dependencies: Option<HashMap<String, Dependency>>,
    pub build: Option<BuildConfig>,
    pub lints: Option<LintsConfig>,
    pub experimental: Option<Experimental>,
}

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct BuildConfig {
    pub level: Option<OptLevel>,
    pub lib_type: Option<LibType>,
    pub std: Option<PathBuf>,
    pub core: Option<PathBuf>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum OptLevel {
    #[serde(rename = "debug")]
    Debug,
    #[serde(rename = "release")]
    Release,
    #[serde(rename = "minsizerel")]
    MinSizeRel,
    #[serde(rename = "relwithdebinfo")]
    RelWithDebInfo,
}

impl Display for OptLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptLevel::Debug => write!(f, "debug"),
            OptLevel::Release => write!(f, "release"),
            OptLevel::MinSizeRel => write!(f, "min-size-rel"),
            OptLevel::RelWithDebInfo => write!(f, "rel-with-deb-info"),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum ProjectType {
    #[serde(rename = "lib")]
    Lib,
    #[serde(rename = "bin")]
    Bin,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum LibType {
    #[serde(rename = "static")]
    Static,
    #[serde(rename = "dynamic")]
    Dynamic,
}

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    pub description: Option<String>,
    pub author: Option<String>,
    pub license: Option<String>,
    pub repository: Option<String>,
    pub homepage: Option<String>,
    pub keywords: Option<Vec<String>>,
    pub r#type: ProjectType,
    pub lib: Option<PathBuf>,
    pub bin: Option<PathBuf>,
}

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct Dependency {
    pub version: Option<String>,
    pub path: Option<String>,
    pub github: Option<String>,
    pub branch: Option<String>,
}

impl Dependency {
    pub fn is_special(name: &str) -> bool {
        matches!(name, "std" | "core")
    }
}

#[derive(Debug, Clone)]
pub struct Config {
    pub project: ProjectConfig,
    pub tasks: Option<HashMap<String, String>>,
    pub dependencies: HashMap<String, Dependency>,
    pub build: ParsedBuildConfig,
    pub lints: LintsConfig,
    pub experimental: Experimental,
    pub cwd: PathBuf,
}

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct ParsedBuildConfig {
    pub level: OptLevel,
    pub lib_type: LibType,
    pub std: PathBuf,
    pub core: PathBuf,
}

impl Config {
    pub fn get(cwd: &PathBuf, args: Option<&ArgMatches>) -> Result<Self> {
        let mut path = walk_for_file(cwd.clone(), "brim.toml").ok_or_else(|| {
            debug!("config file not found in {:?}", cwd);

            ConfigError::ConfigFileNotFound
        })?;
        let content = read_to_string(&path).context("Failed to read brim.toml")?;

        let config: BrimConfig = toml::from_str(&content).context("Failed to parse brim.toml")?;

        let project_type = config.project.r#type.clone();
        if !matches!(project_type, ProjectType::Lib | ProjectType::Bin) {
            return Err(ConfigError::InvalidProjectType(format!("{:?}", project_type)).into());
        }

        path.pop();

        Ok(Self::parse(config, args, path)?)
    }

    pub fn parse(config: BrimConfig, args: Option<&ArgMatches>, cwd: PathBuf) -> Result<Self> {
        let project = config.project;
        let tasks = config.tasks;
        let dependencies = config.dependencies.unwrap_or_default();

        ensure!(!project.name.is_empty(), "Project name can't be empty");
        let brim_path = dirs::home_dir().unwrap().join(".brim");
        let default_std = brim_path.join("std");
        let default_core = brim_path.join("core");

        let mut build = config.build.map_or_else(
            || ParsedBuildConfig {
                level: OptLevel::Debug,
                lib_type: LibType::Static,
                std: default_std.clone(),
                core: default_core.clone(),
            },
            |build| ParsedBuildConfig {
                level: build.level.unwrap_or(OptLevel::Debug),
                lib_type: build.lib_type.unwrap_or(LibType::Static),
                std: build.std.unwrap_or(default_std.clone()),
                core: build.core.unwrap_or(default_core.clone()),
            },
        );

        if let Some(args) = args {
            build.level = if args.get_flag("release") {
                OptLevel::Release
            } else if args.get_flag("min-size-rel") {
                OptLevel::MinSizeRel
            } else if args.get_flag("rel-with-deb-info") {
                OptLevel::RelWithDebInfo
            } else {
                OptLevel::Debug
            };

            build.lib_type = if args.get_flag("dynamic") {
                LibType::Dynamic
            } else {
                LibType::Static
            };
        }

        let lints = config.lints.unwrap_or_default();
        let experimental = config.experimental.unwrap_or_default();

        let std_dep = Dependency {
            version: None,
            path: Some(build.std.to_string_lossy().into_owned()),
            github: None,
            branch: None,
        };

        let core_dep = Dependency {
            version: None,
            path: Some(build.core.to_string_lossy().into_owned()),
            github: None,
            branch: None,
        };

        let dependencies = dependencies;
        // dependencies.insert("std".to_string(), std_dep);
        // dependencies.insert("core".to_string(), core_dep);
        let cwd = canonicalize_path(cwd)?;

        Ok(Self {
            project,
            tasks,
            build,
            dependencies,
            lints,
            experimental,
            cwd,
        })
    }

    pub fn is_bin(&self) -> bool {
        matches!(self.project.r#type, ProjectType::Bin)
    }

    pub fn is_lib(&self) -> bool {
        matches!(self.project.r#type, ProjectType::Lib)
    }

    pub fn binary_path(&self, default: PathBuf) -> PathBuf {
        self.project.bin.clone().unwrap_or(default)
    }

    pub fn library_path(&self, default: PathBuf) -> PathBuf {
        self.project.lib.clone().unwrap_or(default)
    }

    pub fn main_dir(&self) -> PathBuf {
        if self.is_bin() {
            self.binary_path(PathBuf::from("src/main.brim"))
        } else {
            self.library_path(PathBuf::from("src/index.brim"))
        }
    }
}

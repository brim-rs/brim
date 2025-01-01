use anyhow::{Context, Result, ensure};
use brim_fs::walk_dir::walk_for_file;
use clap::{ArgMatches, ColorChoice};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fs::read_to_string, path::PathBuf};
use std::fmt::Display;
use crate::errors::ConfigError;

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct BrimConfig {
    pub project: ProjectConfig,
    pub tasks: Option<HashMap<String, String>>,
    pub dependencies: Option<HashMap<String, Dependency>>,
    pub build: Option<BuildConfig>,
}

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct BuildConfig {
    pub level: Option<OptLevel>,
    pub lib_type: Option<LibType>,
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
    pub r#type: Option<ProjectType>,
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

#[derive(Debug, Clone)]
pub struct Config {
    pub project: ProjectConfig,
    pub tasks: Option<HashMap<String, String>>,
    pub dependencies: HashMap<String, Dependency>,
    pub build: ParsedBuildConfig,
}

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct ParsedBuildConfig {
    pub level: OptLevel,
    pub lib_type: LibType,
}

impl Config {
    pub fn get(cwd: &PathBuf, args: Option<&ArgMatches>) -> Result<Self> {
        let path = walk_for_file(cwd.clone(), "brim.toml").ok_or(ConfigError::ConfigFileNotFound)?;

        let content = read_to_string(&path).context("Failed to read brim.toml")?;

        let config: BrimConfig = toml::from_str(&content).context("Failed to parse brim.toml")?;

        let project_type = config
            .project
            .r#type
            .as_ref()
            .ok_or(ConfigError::MissingProjectType)?;

        if !matches!(project_type, ProjectType::Lib | ProjectType::Bin) {
            return Err(ConfigError::InvalidProjectType(format!("{:?}", project_type)).into());
        }

        Ok(Self::parse(config, args)?)
    }

    pub fn parse(config: BrimConfig, args: Option<&ArgMatches>) -> Result<Self> {
        let project = config.project;
        let tasks = config.tasks;
        let dependencies = config.dependencies;

        ensure!(project.name.len() > 0, "Project name can't be empty");

        let mut build = if let Some(build) = config.build {
            ParsedBuildConfig {
                level: build.level.unwrap_or(OptLevel::Debug),
                lib_type: build.lib_type.unwrap_or(LibType::Static),
            }
        } else {
            ParsedBuildConfig {
                level: OptLevel::Debug,
                lib_type: LibType::Static,
            }
        };

        if let Some(args) = args {
            if args.get_flag("release") {
                build.level = OptLevel::Release;
            } else if args.get_flag("min-size-rel") {
                build.level = OptLevel::MinSizeRel;
            } else if args.get_flag("rel-with-deb-info") {
                build.level = OptLevel::RelWithDebInfo;
            } else {
                build.level = OptLevel::Debug;
            }

            if args.get_flag("dynamic") {
                build.lib_type = LibType::Dynamic;
            } else {
                build.lib_type = LibType::Static;
            }
        }

        Ok(Self {
            project,
            tasks,
            dependencies: dependencies.unwrap_or(HashMap::new()),
            build,
        })
    }
}

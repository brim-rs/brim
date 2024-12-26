use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::PathBuf;
use anyhow::{anyhow, ensure, Context};
use serde::{Deserialize, Serialize};
use crate::{BrimConfig, Dependency, LibType, OptLevel, ProjectConfig, ProjectType};
use anyhow::Result;
use clap::ArgMatches;
use brim_fs::walk_dir::walk_for_file;
use crate::errors::ConfigError;

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct ParsedBrimConfig {
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


impl ParsedBrimConfig {
    pub fn get(cwd: PathBuf, args: Option<&ArgMatches>) -> Result<Self> {
        let path = walk_for_file(cwd, "brim.toml")
            .ok_or(ConfigError::ConfigFileNotFound)?;

        let content = read_to_string(&path)
            .context("Failed to read brim.toml")?;

        let config: BrimConfig = toml::from_str(&content)
            .context("Failed to parse brim.toml")?;

        let project_type = config.project.r#type
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
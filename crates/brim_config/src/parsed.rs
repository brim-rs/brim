use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::PathBuf;
use anyhow::{anyhow, Context};
use serde::{Deserialize, Serialize};
use crate::{BrimConfig, Dependency, LibType, OptLevel, ProjectConfig, ProjectType};
use anyhow::Result;
use brim_fs::walk_dir::walk_for_file;

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
    pub fn get(cwd: PathBuf) -> Result<Self> {
        let path = walk_for_file(cwd.clone(), "brim.toml");

        if path.is_none() {
            return Err(anyhow!("Failed to find 'brim.toml'. Make sure you are running the command inside project root or in a subdirectory"));
        }

        let path = path.unwrap();

        let content = read_to_string(&path).context("Failed to read roan.toml")?;
        let config = toml::from_str::<BrimConfig>(&content)?;

        if config.project.r#type.is_none() {
            return Err(anyhow!(
                "Project type is not specified in [project] in roan.toml. Available types: 'lib', 'bin'"
            ));
        }

        let r#type = config.project.r#type.as_ref().unwrap();
        if r#type != &ProjectType::Lib && r#type != &ProjectType::Bin {
            return Err(anyhow!(
                "Invalid project type in [project] in roan.toml. Available types: 'lib', 'bin'"
            ));
        }

        Ok(Self::parse(config))
    }

    pub fn parse(config: BrimConfig) -> Self {
        let project = config.project;
        let tasks = config.tasks;
        let dependencies = config.dependencies;

        let build = if let Some(build) = config.build {
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

        Self {
            project,
            tasks,
            dependencies: dependencies.expect("REASON"),
            build,
        }
    }
}
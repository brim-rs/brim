use serde::{Deserialize, Serialize};
use std::{collections::HashMap, path::PathBuf};
use std::fmt::Display;

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct BrimConfig {
    pub project: ProjectConfig,
    pub tasks: Option<HashMap<String, String>>,
    pub dependencies: Option<HashMap<String, Dependency>>,
    pub build: Option<BuildConfig>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum BuildType {
    #[serde(rename = "debug")]
    Debug,
    #[serde(rename = "release")]
    Release,
}

impl Display for BuildType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildType::Debug => write!(f, "debug"),
            BuildType::Release => write!(f, "release"),
        }
    }
}

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct BuildConfig {
    pub r#type: Option<BuildType>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum ProjectType {
    #[serde(rename = "lib")]
    Lib,
    #[serde(rename = "bin")]
    Bin,
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

pub mod parsed;
mod errors;

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

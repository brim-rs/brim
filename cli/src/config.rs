use serde::{Deserialize, Serialize};
use std::{collections::HashMap, path::PathBuf};
use crate::compilation::build_type::BuildType;
use crate::compilation::code_gen::linker::LinkerKind;

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct BrimConfig {
    pub project: ProjectConfig,
    pub tasks: Option<HashMap<String, String>>,
    pub dependencies: Option<HashMap<String, Dependency>>,
    pub build: Option<BuildConfig>,
}

#[derive(Deserialize, Debug, Clone, Serialize)]
pub struct BuildConfig {
    pub r#type: Option<BuildType>,
    pub linker: Option<LinkerKind>
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
    pub r#type: Option<String>,
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

use std::fmt::Display;
use clap::ArgMatches;
use serde::{Deserialize, Serialize};
use crate::context::GlobalContext;
use anyhow::Result;

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

pub fn resolve_build_type(ctx: &mut GlobalContext, matches: &ArgMatches) -> Result<BuildType> {
    let from_cli = if matches.get_flag("release") {
        Some(BuildType::Release)
    } else if matches.get_flag("debug") {
        Some(BuildType::Debug)
    } else {
        None
    };

    if let Some(val) = from_cli {
        Ok(val)
    } else {
        if let Some(build) = &ctx.config.build && let Some(r#type) = &build.r#type {
            Ok(r#type.clone())
        } else {
            Ok(BuildType::Debug)
        }
    }
}
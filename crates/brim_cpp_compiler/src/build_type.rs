use std::fmt::Display;
use clap::ArgMatches;
use anyhow::Result;
use brim_config::{BrimConfig, BuildType};

pub fn resolve_build_type(config: &BrimConfig, matches: &ArgMatches) -> Result<BuildType> {
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
        if let Some(build) = &config.build && let Some(r#type) = &build.r#type {
            Ok(r#type.clone())
        } else {
            Ok(BuildType::Debug)
        }
    }
}
#![feature(let_chains)]

use std::ffi::OsStr;
use std::path::Path;
use std::sync::Arc;
use crate::detect::{detect_compiler, resolve_from_kind, Compiler, CompilerKind};
use anyhow::Result;
use brim_config::BuildType;

pub mod detect;
pub mod build_type;

#[derive(Debug)]
pub struct CppBuild {
    pub compiler: Compiler,
    pub files: Vec<Arc<OsStr>>,
    pub flags: Vec<Arc<OsStr>>,
    pub include_dirs: Vec<Arc<Path>>,
}

fn os_str<S: AsRef<OsStr>>(s: S) -> Arc<OsStr> {
    Arc::from(s.as_ref().to_os_string())
}

impl CppBuild {
    pub fn new(
        compiler: Option<CompilerKind>
    ) -> Result<Self> {
        Ok(Self {
            compiler: if let Some(compiler) = compiler {
                resolve_from_kind(compiler)?
            } else {
                detect_compiler()?
            },
            files: vec![],
            flags: vec![],
            include_dirs: vec![],
        })
    }

    pub fn include<P: AsRef<Path>>(&mut self, dir: P) -> &mut Self {
        self.include_dirs.push(dir.as_ref().into());
        self
    }

    pub fn build_type(&mut self, build_type: BuildType) -> &mut Self {
        let flag = match (&self.compiler.kind, build_type) {
            (CompilerKind::Msvc, BuildType::Debug) => os_str("-Zi"),
            (CompilerKind::Msvc, BuildType::Release) => os_str("/O2"),
            (_, BuildType::Debug) => os_str("-g"), // Gcc and Clang
            (_, BuildType::Release) => os_str("-O3"), // Gcc and Clang
        };
        self.flags.push(flag);
        self
    }

    pub fn file<S: AsRef<OsStr>>(&mut self, file: S) -> &mut Self {
        self.files.push(os_str(file));
        self
    }

    pub fn flag<S: AsRef<OsStr>>(&mut self, flag: S) -> &mut Self {
        self.flags.push(os_str(flag));
        self
    }
}
use std::collections::HashMap;
use std::env;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub enum CompilerKind {
    #[serde(rename = "msvc")]
    Msvc,
    #[serde(rename = "gcc")]
    Gcc,
    #[serde(rename = "clang")]
    Clang,
}

#[derive(Debug, Clone)]
pub struct Compiler {
    pub path: PathBuf,
    pub kind: CompilerKind,
}

impl Display for Compiler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            CompilerKind::Msvc => write!(f, "MSVC compiler"),
            CompilerKind::Gcc => write!(f, "GCC compiler"),
            CompilerKind::Clang => write!(f, "Clang compiler"),
        }
    }
}

impl Compiler {
    pub fn new(path: PathBuf, kind: CompilerKind) -> Self {
        Self { path, kind }
    }
}

#[cfg(target_os = "windows")]
pub fn detect_compiler() -> Result<Compiler> {
    let msvc_paths = vec![
        "2022/Community/VC/Auxiliary/Build/vcvars32.bat",
        "2019/Community/VC/Auxiliary/Build/vcvars32.bat",
    ];

    let program_files = env::var("ProgramFiles")?;
    let vs_base = Path::new(&program_files).join("Microsoft Visual Studio");

    for path in msvc_paths {
        let full_path = vs_base.join(path);

        if full_path.exists() {
            let env_vars = setup_msvc_environment(full_path.clone())?;
            apply_env_vars(env_vars);
            let path = which::which("cl")?;
            return Ok(Compiler::new(path, CompilerKind::Msvc));
        }
    }

    if let Ok(path) = which::which("clang++") {
        return Ok(Compiler::new(path, CompilerKind::Clang));
    }

    Err(anyhow!("No suitable compiler found on Windows"))
}

#[cfg(target_os = "linux")]
pub fn detect_compiler() -> Result<Compiler> {
    if let Ok(path) = which::which("g++") {
        return Ok(Compiler::new(path, CompilerKind::Gcc));
    }

    if let Ok(path) = which::which("clang++") {
        return Ok(Compiler::new(path, CompilerKind::Clang));
    }

    Err(anyhow!("No suitable compiler found on Linux"))
}

#[cfg(target_os = "macos")]
pub fn detect_compiler() -> Result<Compiler> {
    if let Ok(path) = which::which("clang++") {
        return Ok(Compiler::new(path, CompilerKind::Clang));
    }

    if let Ok(path) = which::which("gcc++") {
        return Ok(Compiler::new(path, CompilerKind::Gcc));
    }

    Err(anyhow!("No suitable compiler found on macOS"))
}

fn setup_msvc_environment(path_to_msvc: PathBuf) -> Result<HashMap<String, String>> {
    let output = Command::new("cmd")
        .args(&["/C", path_to_msvc.to_str().unwrap(), "x64", "&&", "set"])
        .stdout(Stdio::piped())
        .output()?;

    let mut env_vars = HashMap::new();

    if output.status.success() {
        let output = String::from_utf8_lossy(&output.stdout);

        for line in output.lines() {
            if let Some((key, value)) = line.split_once('=') {
                env_vars.insert(key.to_string(), value.to_string());
            }
        }
    }

    Ok(env_vars)
}

fn apply_env_vars(env_vars: HashMap<String, String>) {
    for (key, value) in env_vars {
        std::env::set_var(key, value);
    }
}

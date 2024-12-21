use std::cmp::PartialEq;
use std::collections::HashMap;
use std::env;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};

pub fn is_msvc_available() -> Result<(bool, Option<PathBuf>)> {
    let program_files = env::var("ProgramFiles")?;
    let vs_base = Path::new(&program_files).join("Microsoft Visual Studio");
    let msvc_paths = vec![
        "2022/Community/VC/Auxiliary/Build/vcvars32.bat",
        "2019/Community/VC/Auxiliary/Build/vcvars32.bat",
    ];

    for path in msvc_paths {
        let full_path = vs_base.join(path);

        if full_path.exists() {
            return Ok((true, Some(full_path)));
        }
    }

    Ok((false, None))
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

pub fn resolve_from_kind(kind: LinkerKind) -> Result<Linker> {
    match kind {
        LinkerKind::Msvc => {
            let (msvc_available, path_to_msvc) = is_msvc_available()?;

            if msvc_available && let Some(path) = path_to_msvc {
                let msvc_path = PathBuf::from(path);

                let env_vars = setup_msvc_environment(msvc_path)?;
                apply_env_vars(env_vars);

                let path = which::which("link")?;

                Ok(Linker::new(path, LinkerKind::Msvc))
            } else {
                Err(anyhow!("MSVC linker is not available"))
            }
        }
        LinkerKind::Ld => {
            let path = which::which("ld")?;

            Ok(Linker::new(path, LinkerKind::Ld))
        }
        LinkerKind::Lld => {
            let name = if cfg!(target_os = "windows") {
                "lld-link"
            } else if cfg!(target_os = "linux") {
                "ld.lld"
            } else {
                "ld64.lld"
            };

            let path = which::which(name)?;

            Ok(Linker::new(path, LinkerKind::Lld))
        }
    }
}

#[cfg(target_os = "windows")]
pub fn detect_linker() -> Result<Linker> {
    let (msvc_available, path) = is_msvc_available()?;

    if msvc_available && let Some(path) = path {
        let msvc_path = PathBuf::from(path);

        let env_vars = setup_msvc_environment(msvc_path)?;
        apply_env_vars(env_vars);

        let path = which::which("link")?;

        Ok(Linker::new(path, LinkerKind::Msvc))
    } else {
        let path = which::which("lld-link")?;

        Ok(Linker::new(path, LinkerKind::Lld))
    }
}

#[cfg(target_os = "linux")]
pub fn detect_linker() -> Result<Linker> {
    let path = which::which("ld")?;

    Ok(Linker::new(path, LinkerKind::Ld))
}

#[cfg(target_os = "macos")]
pub fn detect_linker() -> Result<Linker> {
    let path = which::which("ld")?;

    Ok(Linker::new(path, LinkerKind::Ld))
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
pub enum LinkerKind {
    #[serde(rename = "msvc")]
    Msvc,
    #[serde(rename = "ld")]
    Ld, // GNU ld
    #[serde(rename = "lld")]
    Lld, // LLVM ld
}

#[derive(Debug, Clone)]
pub struct Linker {
    pub path: PathBuf,
    pub kind: LinkerKind,
}

impl Display for Linker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            LinkerKind::Msvc => write!(f, "MSVC linker"),
            LinkerKind::Ld => write!(f, "GNU ld"),
            LinkerKind::Lld => write!(f, "LLVM ld"),
        }
    }
}

impl Linker {
    pub fn new(path: PathBuf, kind: LinkerKind) -> Self {
        Self { path, kind }
    }

    pub fn link(&self, exec_name: String, obj_files: Vec<PathBuf>, cwd: PathBuf) -> Result<()> {
        let mut command = Command::new(&self.path);

        match self.kind {
            LinkerKind::Msvc => {
                command.arg("/NOLOGO")
                    .arg(format!("/OUT:{}", exec_name))
                    .arg("/MACHINE:X64")
                    .arg("/ENTRY:main");
            }
            LinkerKind::Ld | LinkerKind::Lld => {
                command.args(&["-o".to_owned(), exec_name.clone()]);
            }
        }

        let temp_dir = cwd.join("temp_link");
        std::fs::create_dir_all(&temp_dir)?;

        let mut temp_obj_files = Vec::new();
        for obj_file in obj_files {
            let file_name = obj_file.file_name()
                .ok_or_else(|| anyhow!("Invalid object file name"))?;
            let temp_path = temp_dir.join(file_name);
            std::fs::copy(&obj_file, &temp_path)?;
            temp_obj_files.push(temp_path);
        }

        command.current_dir(&temp_dir);

        for obj_file in temp_obj_files {
            let file_name = obj_file.file_name()
                .ok_or_else(|| anyhow!("Invalid object file name"))?;
            command.arg(file_name);
        }

        let output = command.output()?;

        if !output.status.success() {
            let output = if output.stderr.is_empty() {
                output.stdout
            } else {
                output.stderr
            };

            std::fs::remove_dir_all(&temp_dir)?;
            eprintln!("Linker failed: {}", String::from_utf8_lossy(&output));
            return Err(anyhow!("Linker failed"));
        }

        std::fs::rename(temp_dir.join(&exec_name), cwd.join(&exec_name))?;

        std::fs::remove_dir_all(&temp_dir)?;

        Ok(())
    }
}
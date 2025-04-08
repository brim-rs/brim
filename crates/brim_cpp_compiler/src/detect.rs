use crate::compiler::{Compiler, CompilerKind};
use anyhow::{Context, Result, anyhow};
use std::{
    collections::HashMap,
    env,
    path::Path,
    process::{Command, Stdio},
};
use tracing::{debug, warn};

#[cfg(target_os = "windows")]
pub fn detect_compiler() -> Result<Compiler> {
    debug!("Detecting C++ compiler on Windows");
    match detect_msvc_compiler() {
        Ok(compiler) => {
            debug!("Found MSVC compiler: {}", compiler);
            Ok(compiler)
        }
        Err(e) => {
            warn!("MSVC detection failed: {}, falling back to GCC/Clang", e);
            detect_clang_or_gcc()
        }
    }
}

/// Detects available C++ compiler on Linux
#[cfg(target_os = "linux")]
pub fn detect_compiler() -> Result<Compiler> {
    debug!("Detecting C++ compiler on Linux");
    detect_gcc_or_clang()
}

/// Detects available C++ compiler on macOS
#[cfg(target_os = "macos")]
pub fn detect_compiler() -> Result<Compiler> {
    debug!("Detecting C++ compiler on macOS");
    detect_clang_or_gcc()
}

/// Attempts to detect and configure MSVC compiler
fn detect_msvc_compiler() -> Result<Compiler> {
    let msvc_paths = vec![
        "2022/Community/VC/Auxiliary/Build/vcvars32.bat",
        "2019/Community/VC/Auxiliary/Build/vcvars32.bat",
    ];

    let program_files = env::var("ProgramFiles")?;
    let vs_base = Path::new(&program_files).join("Microsoft Visual Studio");

    for path in msvc_paths {
        let full_path = vs_base.join(path);

        if full_path.exists() {
            let env_vars = setup_msvc_environment(&full_path)?;
            apply_env_vars(&env_vars);
            let path = which::which("cl")?;
            return Ok(Compiler::new(path, CompilerKind::Msvc)?);
        }
    }

    Err(anyhow!("MSVC compiler not found"))
}
/// Attempts to detect Clang compiler first, then GCC
fn detect_clang_or_gcc() -> Result<Compiler> {
    debug!("Attempting to detect Clang compiler");
    if let Ok(path) = which::which("clang++") {
        debug!("Found Clang++ at: {}", path.display());
        return Compiler::new(path, CompilerKind::Clang)
            .context("Failed to initialize Clang compiler");
    }

    debug!("Clang not found, attempting to detect GCC");
    if let Ok(path) = which::which("g++") {
        debug!("Found G++ at: {}", path.display());
        return Compiler::new(path, CompilerKind::Gcc).context("Failed to initialize GCC compiler");
    }

    Err(anyhow!("No suitable C++ compiler found"))
}

fn setup_msvc_environment(vcvars_path: &Path) -> Result<HashMap<String, String>> {
    debug!("Setting up MSVC environment using: {}", vcvars_path.display());

    let output = Command::new("cmd")
        .args(&["/C", vcvars_path.to_str().unwrap(), "x64", "&&", "set"])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .context("Failed to execute vcvars script")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow!("vcvars script failed: {}", stderr));
    }

    let mut env_vars = HashMap::new();
    let output_str = String::from_utf8_lossy(&output.stdout);

    for line in output_str.lines() {
        if let Some((key, value)) = line.split_once('=') {
            env_vars.insert(key.to_string(), value.to_string());
        }
    }

    if env_vars.is_empty() {
        return Err(anyhow!("No environment variables were set by vcvars script"));
    }

    debug!("Successfully captured {} environment variables", env_vars.len());
    Ok(env_vars)
}

/// Applies environment variables to the current process
fn apply_env_vars(env_vars: &HashMap<String, String>) {
    for (key, value) in env_vars {
        unsafe { env::set_var(key, value) };
    }
}

/// Resolves a compiler from a specific CompilerKind
pub fn resolve_from_kind(kind: CompilerKind) -> Result<Compiler> {
    debug!("Resolving compiler of kind: {:?}", kind);
    match kind {
        CompilerKind::Msvc => detect_msvc_compiler().context("Failed to resolve MSVC compiler"),
        CompilerKind::Gcc => {
            if let Ok(path) = which::which("g++") {
                Compiler::new(path, CompilerKind::Gcc).context("Failed to initialize GCC compiler")
            } else {
                Err(anyhow!("GCC compiler not found"))
            }
        }
        CompilerKind::Clang => {
            if let Ok(path) = which::which("clang++") {
                Compiler::new(path, CompilerKind::Clang)
                    .context("Failed to initialize Clang compiler")
            } else {
                Err(anyhow!("Clang compiler not found"))
            }
        }
    }
}

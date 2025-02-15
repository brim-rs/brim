use serde::{Deserialize, Serialize};
use std::{
    fmt::{Display, Formatter},
    path::{Path, PathBuf},
    process::Command,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Compiler not found at path: {0}")]
    NotFound(PathBuf),
    #[error("Compiler validation failed: {0}")]
    ValidationFailed(String),
    #[error("Unsupported compiler version: {0}")]
    UnsupportedVersion(String),
    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

/// Supported compiler types
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum CompilerKind {
    Msvc,
    Gcc,
    Clang,
}

impl CompilerKind {
    pub fn default_extension(&self) -> &str {
        match self {
            CompilerKind::Msvc => "exe",
            CompilerKind::Gcc | CompilerKind::Clang => "",
        }
    }

    pub fn is_unix_style(&self) -> bool {
        matches!(self, CompilerKind::Gcc | CompilerKind::Clang)
    }
}

#[derive(Debug, Clone)]
pub struct Compiler {
    path: PathBuf,
    kind: CompilerKind,
    version: Option<String>,
}

impl Display for Compiler {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ({}) at {}",
            match self.kind {
                CompilerKind::Msvc => "MSVC",
                CompilerKind::Gcc => "GCC",
                CompilerKind::Clang => "Clang",
            },
            self.version.as_deref().unwrap_or("unknown version"),
            self.path.display()
        )
    }
}

impl Compiler {
    /// Creates a new compiler instance and validates its existence
    pub fn new(path: PathBuf, kind: CompilerKind) -> Result<Self, CompilerError> {
        if !path.exists() {
            return Err(CompilerError::NotFound(path));
        }

        let mut compiler = Self {
            path,
            kind,
            version: None,
        };

        compiler.detect_capabilities()?;
        Ok(compiler)
    }

    /// Detects compiler version and capabilities
    fn detect_capabilities(&mut self) -> Result<(), CompilerError> {
        let version_flag = match self.kind {
            CompilerKind::Msvc => "/?",
            CompilerKind::Gcc | CompilerKind::Clang => "--version",
        };

        let output = Command::new(&self.path)
            .arg(version_flag)
            .output()
            .map_err(CompilerError::IoError)?;

        if !output.status.success() {
            return Err(CompilerError::ValidationFailed(
                "Failed to get compiler version".to_string(),
            ));
        }

        let output_str = String::from_utf8_lossy(&output.stdout);
        self.version = Some(self.parse_version(&output_str));

        Ok(())
    }

    /// Parses version string from compiler output
    fn parse_version(&self, output: &str) -> String {
        // Implementation would depend on specific compiler output formats
        // This is a simplified version
        output.lines().next().unwrap_or("unknown").to_string()
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn kind(&self) -> &CompilerKind {
        &self.kind
    }

    pub fn version(&self) -> Option<&str> {
        self.version.as_deref()
    }

    pub fn create_command(&self) -> Command {
        let mut cmd = Command::new(&self.path);

        match self.kind {
            CompilerKind::Msvc => {
                cmd.arg("/nologo"); // Suppress startup banner
                cmd.arg("/std:c++20");
            }
            _ => {
                cmd.arg("-std=c++20");
                cmd.arg("-Wall") // Enable all warnings
                    .arg("-Wextra"); // Enable extra warnings
            }
        }

        cmd
    }
}

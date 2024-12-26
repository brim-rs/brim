use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::process::Command;
use serde::{Deserialize, Serialize};
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
    capabilities: CompilerCapabilities,
}

#[derive(Debug, Clone, Default)]
pub struct CompilerCapabilities {
    supports_cpp20: bool,
    supports_modules: bool,
    supports_concepts: bool,
    max_optimization_level: u8,
}

impl Display for Compiler {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ({}{}) at {}",
            match self.kind {
                CompilerKind::Msvc => "MSVC",
                CompilerKind::Gcc => "GCC",
                CompilerKind::Clang => "Clang",
            },
            self.version.as_deref().unwrap_or("unknown version"),
            if self.capabilities.supports_cpp20 { ", C++20" } else { "" },
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
            capabilities: CompilerCapabilities::default(),
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
        self.detect_cpp_features()?;

        Ok(())
    }

    /// Parses version string from compiler output
    fn parse_version(&self, output: &str) -> String {
        // Implementation would depend on specific compiler output formats
        // This is a simplified version
        output.lines().next().unwrap_or("unknown").to_string()
    }

    fn detect_cpp_features(&mut self) -> Result<(), CompilerError> {
        let test_program = r#"
            #if __cplusplus >= 202002L
                #include <concepts>
                int main() { return 0; }
            #else
                #error "C++20 not supported"
            #endif
        "#;

        let temp_dir = tempfile::Builder::new()
            .prefix("compiler-test")
            .tempdir()
            .map_err(CompilerError::IoError)?;

        let test_file = temp_dir.path().join("test.cpp");
        std::fs::write(&test_file, test_program).map_err(CompilerError::IoError)?;

        let output = Command::new(&self.path)
            .args(match self.kind {
                CompilerKind::Msvc => vec!["/std:c++20", "/c"],
                _ => vec!["-std=c++20", "-c"],
            })
            .arg(test_file)
            .output()
            .map_err(CompilerError::IoError)?;

        self.capabilities.supports_cpp20 = output.status.success();
        self.capabilities.supports_concepts = output.status.success();

        self.capabilities.max_optimization_level = match self.kind {
            CompilerKind::Msvc => 2,  // /O2
            _ => 3,  // -O3
        };

        Ok(())
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

    pub fn supports_cpp20(&self) -> bool {
        self.capabilities.supports_cpp20
    }

    pub fn supports_concepts(&self) -> bool {
        self.capabilities.supports_concepts
    }

    pub fn max_optimization_level(&self) -> u8 {
        self.capabilities.max_optimization_level
    }

    pub fn create_command(&self) -> Command {
        let mut cmd = Command::new(&self.path);

        match self.kind {
            CompilerKind::Msvc => {
                cmd.arg("/nologo");  // Suppress startup banner
                if self.capabilities.supports_cpp20 {
                    cmd.arg("/std:c++20");
                }
            }
            _ => {
                if self.capabilities.supports_cpp20 {
                    cmd.arg("-std=c++20");
                }
                cmd.arg("-Wall")  // Enable all warnings
                    .arg("-Wextra");  // Enable extra warnings
            }
        }

        cmd
    }
}
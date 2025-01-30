#![feature(let_chains)]

use crate::{
    compiler::{Compiler, CompilerKind},
    detect::{detect_compiler, resolve_from_kind},
};
use anyhow::{Context, Result, bail, ensure};
use brim_config::toml::{LibType, OptLevel, ProjectType};
use brim_shell::Shell;
use std::{
    collections::HashSet,
    ffi::OsStr,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};
use tracing::debug;

pub mod compiler;
pub mod detect;

#[derive(Debug)]
pub struct CppBuild {
    compiler: Compiler,
    source_files: HashSet<Arc<OsStr>>,
    flags: Vec<Arc<OsStr>>,
    include_dirs: HashSet<Arc<Path>>,
    library_dirs: HashSet<Arc<Path>>,
    libraries: HashSet<Arc<OsStr>>,
    project_type: ProjectType,
    build_dir: Arc<Path>,
    opt_level: OptLevel,
    defines: HashSet<Arc<OsStr>>,
    lib_type: LibType,
}

impl CppBuild {
    pub fn new(
        compiler: Option<CompilerKind>,
        project_type: ProjectType,
        build_dir: impl AsRef<Path>,
        lib_type: LibType,
    ) -> Result<Self> {
        let build_dir = build_dir.as_ref();
        ensure!(
            build_dir.exists(),
            "Build directory does not exist: {}",
            build_dir.display()
        );

        let compiler = if let Some(kind) = compiler {
            resolve_from_kind(kind)?
        } else {
            detect_compiler()?
        };
        Ok(Self {
            compiler,
            source_files: HashSet::new(),
            flags: Vec::new(),
            include_dirs: HashSet::new(),
            library_dirs: HashSet::new(),
            libraries: HashSet::new(),
            project_type,
            build_dir: Arc::from(build_dir),
            opt_level: OptLevel::Debug,
            defines: HashSet::new(),
            lib_type,
        })
    }

    pub fn compiler_kind(&self) -> &CompilerKind {
        self.compiler.kind()
    }

    /// Adds a source file to the build
    pub fn add_source<P: AsRef<OsStr>>(&mut self, path: P) -> &mut Self {
        self.source_files.insert(Arc::from(path.as_ref()));
        self
    }

    /// Adds multiple source files to the build
    pub fn add_sources<I, P>(&mut self, paths: I) -> &mut Self
    where
        I: IntoIterator<Item = P>,
        P: AsRef<OsStr>,
    {
        for path in paths {
            self.add_source(path);
        }
        self
    }

    /// Adds an include directory
    pub fn add_include<P: AsRef<Path>>(&mut self, dir: P) -> &mut Self {
        self.include_dirs.insert(Arc::from(dir.as_ref()));
        self
    }

    /// Adds a library directory
    pub fn add_library_dir<P: AsRef<Path>>(&mut self, dir: P) -> &mut Self {
        self.library_dirs.insert(Arc::from(dir.as_ref()));
        self
    }

    /// Adds a library to link against
    pub fn add_library<S: AsRef<OsStr>>(&mut self, lib: S) -> &mut Self {
        self.libraries.insert(Arc::from(lib.as_ref()));
        self
    }

    /// Adds a compiler flag
    pub fn add_flag<S: AsRef<OsStr>>(&mut self, flag: S) -> &mut Self {
        self.flags.push(Arc::from(flag.as_ref()));
        self
    }

    /// Sets the optimization level
    pub fn set_opt_level(&mut self, level: OptLevel) -> &mut Self {
        self.opt_level = level;
        self
    }

    /// Adds a preprocessor definition
    pub fn define<S: AsRef<OsStr>>(&mut self, define: S) -> &mut Self {
        self.defines.insert(Arc::from(define.as_ref()));
        self
    }

    /// Compiles the project with the current configuration
    pub fn compile(&self, output_name: impl AsRef<OsStr>, shell: &mut Shell) -> Result<PathBuf> {
        ensure!(!self.source_files.is_empty(), "No source files specified");

        let mut command = Command::new(&self.compiler.path());
        self.configure_command(&mut command, output_name.as_ref())?;

        debug!("Executing compilation command: {:?}", command);

        let output = command
            .current_dir(&*self.build_dir)
            .output()
            .context("Failed to execute compiler")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);

            shell.centered("Output from stderr", stderr, true)?;
            shell.centered("Output from stdout", stdout, true)?;

            bail!("Compilation failed");
        }

        let output_path = self.get_output_path(output_name.as_ref());
        ensure!(
            output_path.exists(),
            "Compilation succeeded but output file was not created: {}",
            output_path.display()
        );

        Ok(output_path)
    }

    /// Configures the compiler command with all build settings
    fn configure_command(&self, command: &mut Command, output_name: &OsStr) -> Result<()> {
        for file in &self.source_files {
            command.arg(file);
        }

        for dir in &self.include_dirs {
            command.arg(if self.compiler.kind() == &CompilerKind::Msvc {
                "/I"
            } else {
                "-I"
            });
            command.arg(dir.as_os_str());
        }

        for dir in &self.library_dirs {
            command.arg(if self.compiler.kind() == &CompilerKind::Msvc {
                "/LIBPATH:"
            } else {
                "-L"
            });
            command.arg(dir.as_os_str());
        }

        for lib in &self.libraries {
            command.arg(if self.compiler.kind() == &CompilerKind::Msvc {
                "/DEFAULTLIB:"
            } else {
                "-l"
            });
            command.arg(lib);
        }

        for define in &self.defines {
            command.arg(if self.compiler.kind() == &CompilerKind::Msvc {
                "/D"
            } else {
                "-D"
            });
            command.arg(define);
        }

        self.add_optimization_flags(command);
        self.add_cpp_standard_flag(command)?;
        self.configure_output(command, output_name)?;

        // Add custom flags last to allow overrides
        for flag in &self.flags {
            command.arg(flag);
        }

        Ok(())
    }

    fn add_optimization_flags(&self, command: &mut Command) {
        let (msvc_flag, unix_flag) = match self.opt_level {
            OptLevel::Debug => ("/Od", "-O0"),
            OptLevel::Release => ("/O2", "-O3"),
            OptLevel::MinSizeRel => ("/O1", "-Os"),
            OptLevel::RelWithDebInfo => ("/O2 /Zi", "-O2 -g"),
        };

        command.arg(if self.compiler.kind() == &CompilerKind::Msvc {
            msvc_flag
        } else {
            unix_flag
        });
    }

    fn add_cpp_standard_flag(&self, command: &mut Command) -> Result<()> {
        let flag = if self.compiler.kind() == &CompilerKind::Msvc {
            format!("/std:c++{}", "latest")
        } else {
            format!("-std=c++{}", 23)
        };

        command.arg(flag);
        Ok(())
    }

    fn configure_output(&self, command: &mut Command, output_name: &OsStr) -> Result<()> {
        match self.project_type {
            ProjectType::Lib => match self.lib_type {
                LibType::Static => {
                    if self.compiler.kind() == &CompilerKind::Msvc {
                        command.arg("/c");
                        command.arg(format!("/Fo:{}", output_name.to_string_lossy()));
                    } else {
                        command.arg("-c");
                        command.arg("-o").arg(output_name);
                    }
                }
                LibType::Dynamic => {
                    if self.compiler.kind() == &CompilerKind::Msvc {
                        command.arg("/LD");
                        command.arg(format!("/Fe:{}", output_name.to_string_lossy()));
                    } else {
                        command.arg("-shared");
                        command.arg("-o").arg(if cfg!(windows) {
                            PathBuf::from(output_name).with_extension("dll")
                        } else {
                            output_name.into()
                        });
                    }
                }
            },
            ProjectType::Bin => {
                if self.compiler.kind() == &CompilerKind::Msvc {
                    command.arg(format!("/Fe:{}", output_name.to_string_lossy()));
                } else {
                    command.arg("-o").arg(if cfg!(windows) {
                        PathBuf::from(output_name).with_extension("exe")
                    } else {
                        output_name.into()
                    });
                }
            }
        }
        Ok(())
    }

    pub fn disable_warnings(&mut self) -> &mut Self {
        self.add_flag(if self.compiler.kind() == &CompilerKind::Msvc {
            "/W0"
        } else {
            "-w"
        });
        self
    }

    fn get_output_path(&self, output_name: &OsStr) -> PathBuf {
        self.build_dir
            .join(output_name)
            .with_extension(match self.project_type {
                ProjectType::Lib => match self.lib_type {
                    LibType::Static => "lib",
                    LibType::Dynamic => "dll",
                },
                ProjectType::Bin => {
                    if cfg!(windows) {
                        "exe"
                    } else {
                        ""
                    }
                }
            })
    }
}

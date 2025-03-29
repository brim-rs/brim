use anyhow::{Context, Result};
use brim::Shell;
use clap::{Arg, ArgAction, ArgMatches, Command};
use std::{
    env,
    fmt::{Display, Formatter, Result as FmtResult},
    fs::{self},
    path::{Path, PathBuf},
    process::Command as ProcessCommand,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ProjectType {
    Bin,
    Lib,
}

impl Display for ProjectType {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            ProjectType::Bin => write!(f, "binary"),
            ProjectType::Lib => write!(f, "library"),
        }
    }
}

impl ProjectType {
    fn default_content(&self) -> &'static str {
        match self {
            ProjectType::Bin => BIN_CONTENT,
            ProjectType::Lib => LIB_CONTENT,
        }
    }

    fn default_filename(&self) -> &'static str {
        match self {
            ProjectType::Bin => "main.brim",
            ProjectType::Lib => "index.brim",
        }
    }
}

pub struct ProjectInitializer<'a> {
    shell: &'a mut Shell,
    name: String,
    project_type: ProjectType,
    force: bool,
    no_git: bool,
}

impl<'a> ProjectInitializer<'a> {
    pub fn new(shell: &'a mut Shell, args: &ArgMatches) -> Result<Self> {
        let name = args
            .get_one::<String>("name")
            .context("Project name is required")?
            .to_owned();

        let project_type = match (args.get_flag("bin"), args.get_flag("lib")) {
            (true, false) => ProjectType::Bin,
            (false, true) => ProjectType::Lib,
            (false, false) => ProjectType::Bin,
            (true, true) => anyhow::bail!("Cannot create both binary and library project"),
        };

        Ok(Self {
            shell,
            name,
            project_type,
            force: args.get_flag("force"),
            no_git: args.get_flag("no-git"),
        })
    }

    pub fn init(&mut self) -> Result<()> {
        self.validate_project_name()?;
        let project_dir = self.prepare_project_directory()?;

        if !self.no_git {
            self.initialize_git(&project_dir)?;
            self.create_gitignore(&project_dir)?;
        }

        self.create_project_config(&project_dir)?;
        self.create_source_files(&project_dir)?;

        Ok(())
    }

    fn validate_project_name(&mut self) -> Result<()> {
        if self.name == "std" {
            self.shell
                .warn("'std' is a part of the standard library and not recommended")?;
        }

        if self.name.chars().any(|ch| ch > '\x7f') {
            self.shell
                .warn("Project name contains non-ascii characters")?;
        }

        Ok(())
    }

    fn prepare_project_directory(&mut self) -> Result<PathBuf> {
        let project_dir = env::current_dir()?.join(&self.name);

        if project_dir.exists() {
            if self.force {
                self.shell
                    .warn("Force flag is enabled. Removing existing directory.")?;
                fs::remove_dir_all(&project_dir)?;
            } else {
                anyhow::bail!("Project directory already exists");
            }
        }

        self.shell
            .status("Creating", format!("{} project", self.project_type))?;

        fs::create_dir(&project_dir)?;
        Ok(project_dir)
    }

    fn initialize_git(&mut self, project_dir: &Path) -> Result<()> {
        self.shell.status("Initializing", "git repository")?;

        let output = ProcessCommand::new("git")
            .arg("init")
            .current_dir(project_dir)
            .output()?;

        if !output.status.success() {
            let error_msg = String::from_utf8_lossy(&output.stderr);
            self.shell
                .error(format!("Git initialization failed: {}", error_msg))?;
        }

        Ok(())
    }

    fn create_gitignore(&mut self, project_dir: &Path) -> Result<()> {
        let gitignore_path = project_dir.join(".gitignore");
        self.shell.status("Creating", ".gitignore")?;
        fs::write(&gitignore_path, GITIGNORE)?;
        Ok(())
    }

    fn create_project_config(&mut self, project_dir: &Path) -> Result<()> {
        let mut config = toml_edit::DocumentMut::new();
        config["project"] = toml_edit::Item::Table(toml_edit::Table::default());
        config["project"]["name"] = toml_edit::value(&self.name);
        config["project"]["version"] = toml_edit::value("0.1.0");
        config["project"]["type"] = toml_edit::value(match self.project_type {
            ProjectType::Bin => "bin",
            ProjectType::Lib => "lib",
        });

        let brim_toml = project_dir.join("brim.toml");
        self.shell.status("Creating", "brim.toml")?;
        fs::write(&brim_toml, config.to_string())?;

        Ok(())
    }

    fn create_source_files(&mut self, project_dir: &Path) -> Result<()> {
        let src_dir = project_dir.join("src");
        self.shell.status("Creating", "source files")?;
        fs::create_dir(&src_dir)?;

        let source_path = src_dir.join(self.project_type.default_filename());
        fs::write(&source_path, self.project_type.default_content())?;

        Ok(())
    }
}

pub fn init_cmd() -> Command {
    Command::new("init")
        .about("Initialize a new project")
        .arg(
            Arg::new("name")
                .required(true)
                .help("The name of the project"),
        )
        .arg(
            Arg::new("bin")
                .short('b')
                .long("bin")
                .help("Create a binary project")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("lib")
                .short('l')
                .long("lib")
                .help("Create a library project")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("force")
                .short('f')
                .long("force")
                .help("Force initialization even if the directory is not empty")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no-git")
                .long("no-git")
                .help("Do not initialize git repository")
                .action(ArgAction::SetTrue),
        )
}

pub fn init_command(shell: &mut Shell, args: &ArgMatches) -> Result<()> {
    let mut initializer = ProjectInitializer::new(shell, args)?;
    initializer.init()
}

const GITIGNORE: &str = r#"
# Build and temporary files
/target
*.log
.cache

# Environment files
.env
.env.local

# IDE and editor files
.vscode/
.idea/
*.swp
*.swo

# OS-specific files
.DS_Store
Thumbs.db
"#;

const BIN_CONTENT: &str = r#"
use { println } from std::debug;

pub fn main() {
    println("Hello, world!");
};
"#;

const LIB_CONTENT: &str = r#"
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
"#;

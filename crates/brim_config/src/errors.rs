#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error(
        "Failed to find 'brim.toml'. Make sure you are running the command inside project root or in a subdirectory"
    )]
    ConfigFileNotFound,

    #[error("Failed to read brim.toml: {0}")]
    ConfigReadError(#[from] std::io::Error),

    #[error("Failed to parse TOML: {0}")]
    TomlParseError(#[from] toml::de::Error),

    #[error("Project type is not specified in [project] section")]
    MissingProjectType,

    #[error("Invalid project type '{0}'. Available types: 'lib', 'bin'")]
    InvalidProjectType(String),
}

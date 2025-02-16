use anyhow::{Result, anyhow};
use std::path::{MAIN_SEPARATOR, PathBuf};

pub mod loader;
pub mod walk_dir;

/// Makes it easy to specify the path with platform-specific separators.
///
/// # Examples
/// ```rust
/// use brim_fs::path;
///
/// let p = path(vec!["src", "main.brim"]);
/// ```
pub fn path<P: AsRef<std::path::Path>>(elems: Vec<P>) -> PathBuf {
    let mut path = PathBuf::new();

    for elem in elems {
        path.push(elem);
    }

    path
}

/// Takes a provided path and creates all the parent directories.
pub fn create_file_parent_dirs(file: &PathBuf) -> anyhow::Result<()> {
    if let Some(parent) = file.parent() {
        std::fs::create_dir_all(parent)?;
    } else {
        return Err(anyhow!("No parent directory"));
    }
    Ok(())
}

pub fn remove_prefix(path: &PathBuf) -> PathBuf {
    let path_str = path.to_str().unwrap_or("");
    let normalized_str = if path_str.starts_with(r"\\?\") {
        &path_str[4..] // Strip the \\?\ prefix
    } else {
        path_str
    };
    PathBuf::from(normalized_str)
}

pub fn normalize_path(path: &PathBuf, root: &PathBuf) -> PathBuf {
    let mut temp = path.clone();

    if temp.is_relative() {
        temp = root.join(temp);
    }
    temp = normalize_slashes(&temp);
    remove_prefix(&temp)
}

pub const SEP: char = std::path::MAIN_SEPARATOR;

pub fn normalize_slashes(path: &PathBuf) -> PathBuf {
    let path_str = path.to_str().unwrap_or("");
    let sep = &SEP.to_string();
    let normalized_str = if cfg!(windows) {
        path_str.replace("\\", sep).replace("/", sep)
    } else {
        path_str.to_string()
    };
    PathBuf::from(normalized_str)
}

pub fn canonicalize_path(path: PathBuf) -> Result<PathBuf> {
    let path = path.canonicalize()?;
    Ok(remove_prefix(&path))
}
use anyhow::{Result, anyhow};
use std::path::{Path, PathBuf};

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
pub fn create_file_parent_dirs(file: &Path) -> Result<()> {
    if let Some(parent) = file.parent() {
        std::fs::create_dir_all(parent)?;
    } else {
        return Err(anyhow!("No parent directory"));
    }
    Ok(())
}

pub fn remove_prefix(path: &Path) -> PathBuf {
    let path_str = path.to_str().unwrap_or("");
    PathBuf::from(if let Some(stripped) = path_str.strip_prefix(r"\\?\") {
        &stripped
    } else {
        path_str
    })
}

pub fn normalize_path(path: &Path, root: &Path) -> PathBuf {
    let mut temp = path.to_path_buf();

    if temp.is_relative() {
        temp = root.join(temp);
    }
    temp = normalize_slashes(&temp);
    remove_prefix(&temp)
}

pub const SEP: char = std::path::MAIN_SEPARATOR;

pub fn normalize_slashes(path: &Path) -> PathBuf {
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
    let path = fs_err::canonicalize(path)?;
    Ok(remove_prefix(&path))
}

pub fn paths_equal<P: AsRef<Path>, Q: AsRef<Path>>(a: P, b: Q) -> bool {
    let a = Path::new(a.as_ref()).components().collect::<Vec<_>>();
    let b = Path::new(b.as_ref()).components().collect::<Vec<_>>();
    a == b
}

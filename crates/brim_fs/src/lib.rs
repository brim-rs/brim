use std::path::PathBuf;

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

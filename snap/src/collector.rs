use std::env::current_dir;
use std::path::PathBuf;
use anyhow::{bail, Result};
use glob::glob;
use brim::files::SimpleFiles;

pub fn collect_files() -> Result<SimpleFiles> {
    let path = current_dir()?.join("tests");

    if !path.exists() {
        bail!("Test directory doesn't exists at {}", path.display());
    }

    let mut files = SimpleFiles::new();
    let path = format!("{}/**/*.brim", path.to_string_lossy());
    for entry in glob(&path).expect("Failed to read glob pattern") {
        let file_path = entry?;

        files.add(file_path.clone(), std::fs::read_to_string(&file_path)?);
    }

    Ok(files)
}

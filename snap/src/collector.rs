use anyhow::{Result, bail};
use brim::files::{SimpleFiles, add_file};
use glob::glob;
use std::{env::current_dir, path::PathBuf};

pub fn collect_files() -> Result<()> {
    let path = current_dir()?.join("tests");

    if !path.exists() {
        bail!("Test directory doesn't exists at {}", path.display());
    }

    let path = format!("{}/**/*.brim", path.to_string_lossy());
    for entry in glob(&path).expect("Failed to read glob pattern") {
        let file_path = entry?;

        add_file(file_path.clone(), std::fs::read_to_string(&file_path)?);
    }

    Ok(())
}

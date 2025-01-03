use std::path::PathBuf;
use anyhow::Result;
use brim::files::SimpleFiles;

pub fn run_tests(exec: PathBuf, files: SimpleFiles) -> Result<()> {
    for file in files {}

    Ok(())
}
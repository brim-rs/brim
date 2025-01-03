mod collector;
mod runner;

use anyhow::{bail, Result};
use brim_shell::Shell;
use crate::collector::collect_files;

fn main() -> Result<()> {
    let mut shell = Shell::default();
    match run() {
        Ok(_) => Ok(()),
        Err(e) => {
            shell.error(format!("{}", e))?;
            std::process::exit(1);
        }
    }
}

fn run() -> Result<()> {
    let files = collect_files()?;

    // We will use local version of brim to run the tests. We look for the executable in the target/debug directory.
    let brim = std::env::current_dir()?.join("target").join("debug").join(if cfg!(windows) { "brim-cli.exe" } else { "brim-cli" });
    if !brim.exists() {
        bail!("Brim executable not found at {:?}. Make sure to build it before with `cargo build -p brim-cli`", brim);
    }

    Ok(())
}

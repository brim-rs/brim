mod collector;
mod runner;

use anyhow::{bail, Result};
use brim_shell::Shell;
use crate::collector::collect_files;
use crate::runner::run_tests;

fn main() -> Result<()> {
    let mut shell = &mut Shell::default();
    match run(shell) {
        Ok(_) => Ok(()),
        Err(e) => {
            shell.error(format!("{}", e))?;
            std::process::exit(1);
        }
    }
}

fn run(shell: &mut Shell) -> Result<()> {
    collect_files()?;

    run_tests(shell)?;

    Ok(())
}

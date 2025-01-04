mod collector;
mod runner;

use crate::{collector::collect_files, runner::run_tests};
use anyhow::{Result, bail};
use brim_shell::Shell;

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

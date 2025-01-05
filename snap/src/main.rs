mod collector;
mod reporter;
mod runner;

use crate::{collector::collect_files, runner::run_tests};
use anyhow::Result;
use brim_shell::Shell;
use std::time::Instant;

fn main() -> Result<()> {
    let shell = &mut Shell::default();
    match run(shell) {
        Ok(_) => Ok(()),
        Err(e) => {
            shell.error(format!("{}", e))?;
            std::process::exit(1);
        }
    }
}

fn run(shell: &mut Shell) -> Result<()> {
    let start = Instant::now();

    collect_files()?;
    run_tests(shell, start)?;

    Ok(())
}

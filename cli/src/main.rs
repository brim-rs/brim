#![feature(let_chains)]

use crate::{commands::run::run_command, panic::setup_panic_handler};
use anstream::ColorChoice;
use anyhow::Result;
use brim::{compiler::CompilerContext, session::Session, toml::Config};
use clap::ArgMatches;
use cli::cli;
use std::{env, process::exit};
use brim::args::RunArgs;

pub mod cli;
mod commands;
mod panic;
mod plural;

fn main() -> Result<()> {
    setup_panic_handler();
    let args = cli().try_get_matches().unwrap_or_else(|err| {
        err.print().expect("Error printing error");
        exit(1);
    });
    let verbose = args.get_flag("verbose");

    unsafe { env::set_var("BRIM_LOG", if verbose { "trace" } else { "info" }) };
    // setup_tracing(verbose);

    let cmd = match args.subcommand() {
        Some((cmd, args)) => (cmd, args),
        None => {
            cli().print_help()?;

            return Ok(());
        }
    };

    let color_choice = if args.get_flag("no-color") {
        ColorChoice::Never
    } else {
        ColorChoice::Auto
    };
    let dir = env::current_dir()?;
    match cmd.0 {
        "run" => {
            let config = Config::get(&dir, Some(&cmd.1))?;
            let run_args = RunArgs::from_args(&cmd.1);

            let comp = &mut CompilerContext::new(run_args.clone());
            let sess = &mut Session::new(dir, config, color_choice);

            if args.get_flag("time") {
                sess.measure_time = true;
            }

            exec_command(sess, comp, run_args, run_command)?;
        }
        _ => {
            eprintln!("Unknown command: {}", cmd.0);
            exit(1);
        }
    }

    Ok(())
}

pub fn exec_command<'a>(
    sess: &'a mut Session,
    comp: &'a mut CompilerContext,
    args: RunArgs,
    func: impl FnOnce(&mut Session, &'a mut CompilerContext, ColorChoice, RunArgs) -> Result<()>,
) -> Result<()> {
    match func(sess, comp, sess.color_choice, args) {
        Ok(_) => Ok(()),
        Err(err) => {
            sess.shell().error(err)?;
            exit(1);
        }
    }
}

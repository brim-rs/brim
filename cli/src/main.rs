#![feature(let_chains)]

use crate::{
    commands::{init::init_command, run::run_command},
    panic::setup_panic_handler,
    tracing::setup_tracing,
};
use anstream::ColorChoice;
use anyhow::Result;
use brim::{
    Shell, args::RunArgs, compiler::CompilerContext, lints::Lints, session::Session, toml::Config,
};
use cli::cli;
use std::{env, process::exit};

pub mod cli;
mod commands;
mod panic;
pub mod plural;
mod tracing;

fn main() -> Result<()> {
    let args = cli().try_get_matches().unwrap_or_else(|err| {
        err.print().expect("Error printing error");
        exit(1);
    });
    setup_panic_handler(args.get_flag("no-backtrace"));
    let verbose = args.get_flag("verbose");

    unsafe { env::set_var("BRIM_LOG", if verbose { "trace" } else { "info" }) };
    setup_tracing();

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
    let shell = &mut Shell::new(color_choice);
    let dir = env::current_dir()?;
    let res = match cmd.0 {
        "run" => {
            let config = Config::get(&dir, Some(&cmd.1))?;
            let run_args = RunArgs::from_args(&cmd.1);

            let lints = Box::new(Lints::configure(&config.lints));
            let lints: &'static Lints = Box::leak(lints);
            let mut comp = CompilerContext::new(run_args.clone(), lints);
            let mut sess = Session::new(dir, config, color_choice);

            if args.get_flag("time") {
                sess.measure_time = true;
            }

            exec_command(&mut sess, &mut comp, run_args, run_command)
        }
        "init" => init_command(shell, &cmd.1),
        _ => {
            eprintln!("Unknown command: {}", cmd.0);
            exit(1);
        }
    };

    match res {
        Ok(_) => Ok(()),
        Err(err) => {
            shell.error(err)?;
            exit(1);
        }
    }
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

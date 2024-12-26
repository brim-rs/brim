#![feature(let_chains)]

use crate::{commands::run::run_command, context::GlobalContext};
use anstream::ColorChoice;
use anyhow::Result;
use clap::ArgMatches;
use cli::cli;
use panic_handler::setup_panic_handler;
use std::{env, process::exit};
use ::tracing::debug;
use anstyle::Color;
use brim_shell::Shell;
use crate::tracing::setup_tracing;

pub mod cli;
pub mod commands {
    pub mod run;
}
mod ast;
mod compilation;
pub mod context;
mod error;
mod idx;
pub mod lexer;
pub mod panic_handler;
mod parser;
pub mod path;
mod tracing;
pub mod random;

#[tokio::main]
async fn main() -> Result<()> {
    setup_panic_handler();
    let args = cli().try_get_matches().unwrap_or_else(|err| {
        err.print().expect("Error printing error");
        exit(1);
    });
    let verbose = args.get_flag("verbose");

    env::set_var("BRIM_LOG", if verbose { "trace" } else { "info" });
    setup_tracing(verbose);

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

    match execute_command(cmd, shell, color_choice).await {
        Ok(()) => Ok(()),
        Err(err) => {
            shell.error(&format!("{}", err))?;

            exit(1);
        }
    }
}

pub async fn execute_command(cmd: (&str, &ArgMatches), shell: &mut Shell, color_choice: ColorChoice) -> Result<()> {
    match cmd.0 {
        "run" => {
            let mut ctx = &mut match GlobalContext::default(color_choice, Some(&cmd.1)) {
                Ok(ctx) => ctx,
                Err(err) => {
                    if let Some(toml_err) = err.downcast_ref::<toml::de::Error>() {
                        shell.error(&format!("Failed to parse 'brim.toml': {} \n", toml_err))?;
                    } else {
                        shell.error(&format!("{}", err))?;
                    }
                    exit(1);
                }
            };

            debug!("Running in context: {:#?}", ctx);

            run_command(ctx, cmd.1, shell)?;
            Ok(())
        }
        _ => {
            cli().print_help()?;
            exit(1);
        }
    }
}

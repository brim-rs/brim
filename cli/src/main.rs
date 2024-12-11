#![feature(let_chains)]

use crate::{
    context::GlobalContext,
};
use anstream::ColorChoice;
use anyhow::Result;
use clap::ArgMatches;
use cli::cli;
use panic_handler::setup_panic_handler;
use std::{env, process::exit};
use crate::commands::run::run_command;

pub mod cli;
pub mod commands {
    pub mod run;
}
pub mod context;
pub mod fs;
pub mod panic_handler;
pub mod path;
pub mod lexer;
mod compilation;
mod error;

#[tokio::main]
async fn main() -> Result<()> {
    setup_panic_handler();
    let args = cli().try_get_matches().unwrap_or_else(|err| {
        err.print().expect("Error printing error");
        exit(1);
    });
    let verbose = args.get_flag("verbose");

    env::set_var("BRIM_LOG", if verbose { "trace" } else { "info" });

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

    let mut ctx = GlobalContext::default(color_choice)?;
    ctx.verbose = verbose;

    match execute_command(&mut ctx, cmd).await {
        Ok(()) => Ok(()),
        Err(err) => {
            ctx.shell.error(&format!("{}", err))?;

            exit(1);
        }
    }
}

pub async fn execute_command(ctx: &mut GlobalContext, cmd: (&str, &ArgMatches)) -> Result<()> {
    match cmd.0 {
        "run" => run_command(ctx, cmd.1),
        _ => {
            cli().print_help()?;
            exit(1);
        }
    }
}

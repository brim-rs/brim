#![feature(let_chains)]

use cli::cli;
use std::{env, process::exit};
use anstream::ColorChoice;
use anyhow::Result;
use brim::session::Session;
use brim::toml::Config;

pub mod cli;
mod commands;

fn main() -> Result<()> {
    // setup_panic_handler();
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
            let sess = &mut Session::new(dir, config, color_choice);

            commands::run::run_command(sess, cmd.1)?;
        }
        _ => {
            eprintln!("Unknown command: {}", cmd.0);
            exit(1);
        }
    }

    Ok(())
}

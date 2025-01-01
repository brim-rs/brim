use crate::{
    cli::{
        debug_mode, dynamic_lib_mode, min_size_rel_mode, opt, rel_with_deb_info_mode, release_mode,
        static_lib_mode,
    },
};
use anyhow::Result;
use clap::{ArgAction, ArgMatches, Command};
use std::{process, sync::Arc};
use tracing::debug;
use brim::session::Session;

pub fn run_cmd() -> Command {
    Command::new("run")
        .about("Run a project")
        .arg(
            opt("time", "Prints the time taken to run the project")
                .short('t')
                .action(ArgAction::SetTrue),
        )
        .arg(release_mode())
        .arg(debug_mode())
        .arg(min_size_rel_mode())
        .arg(rel_with_deb_info_mode())
        .arg(dynamic_lib_mode())
        .arg(static_lib_mode())
        .trailing_var_arg(true)
        .arg(
            clap::Arg::new("args")
                .num_args(0..)
                .allow_negative_numbers(true)
                .trailing_var_arg(true),
        )
}

pub fn run_command(sess: &mut Session, args: &ArgMatches) -> Result<()> {
    let start = sess.start;
    let time = args.get_flag("time");

    if time {
        let elapsed = start.elapsed();
        sess.shell().status("Time taken", &format!("{:?}", elapsed))?;
    }

    Ok(())
}


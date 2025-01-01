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
use brim::toml::ProjectType;
use brim_parser::parser_from_simple_file;

pub fn run_cmd() -> Command {
    Command::new("run")
        .about("Run a project")
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
    sess.measure_time(|sess| {
        sess.assert_type(ProjectType::Bin, "Can only use `run` command on binary projects")?;
        let main_file = sess.main_file()?;
        let source_file = sess.get_file(main_file).unwrap();

        let mut parser = parser_from_simple_file(sess, source_file)?;
        let barrel = parser.parse_barrel()?;
        
        Ok(())
    }, "to execute project")?;

    Ok(())
}


use crate::cli::{
    debug_mode, dynamic_lib_mode, min_size_rel_mode, rel_with_deb_info_mode, release_mode,
    static_lib_mode,
};
use anyhow::Result;
use brim::{compiler::CompilerContext, resolver::Resolver, session::Session, toml::ProjectType};
use brim_parser::parser::Parser;
use clap::Command;

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

pub fn run_command<'a>(sess: &mut Session, comp: &'a mut CompilerContext<'a>) -> Result<()> {
    sess.measure_time(
        |sess| {
            sess.assert_type(
                ProjectType::Bin,
                "Can only use `run` command on binary projects",
            )?;

            let main_file = sess.main_file()?;

            let mut parser = Parser::new(main_file);
            let mut barrel = parser.parse_barrel(comp)?;
            let resolver = &mut Resolver::new(comp);

            sess.resolve_and_analyze(&mut barrel, resolver)?;

            for diag in parser.diags.dcx.diags {
                resolver.ctx.emit_diag(diag);
            }

            Ok(())
        },
        "to execute project",
    )?;

    Ok(())
}

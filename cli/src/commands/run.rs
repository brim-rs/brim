use crate::{
    cli::{
        debug_mode, dynamic_lib_mode, min_size_rel_mode, opt, rel_with_deb_info_mode, release_mode,
        static_lib_mode,
    },
    plural::plural,
};
use anyhow::{Result, bail};
use brim::{
    compiler::CompilerContext,
    files::{SimpleFiles, files},
    resolver::Resolver,
    session::Session,
    toml::ProjectType,
};
use brim_parser::parser::Parser;
use clap::{ArgAction, Command};
use std::collections::HashSet;

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
        // when true display generated c++ code
        .arg(
            opt(
                "codegen-debug",
                "Displays generated c++ code in the terminal",
            )
            .short('c')
            .action(ArgAction::SetTrue),
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
            for diag in &parser.diags.dcx.diags {
                comp.emit_diag(diag.clone());
            }

            let mut resolver = Resolver::new(comp);
            let mut visited = HashSet::new();
            let module_map = resolver.create_module_map(&mut barrel, &mut visited)?;

            let (hir, comp) = sess.analyze(module_map, resolver.ctx)?;

            let emitted = comp.emitted.len();
            if emitted > 0 {
                bail!(
                    "Compilation failed due to {} {}",
                    emitted,
                    plural(emitted, "error", "errors")
                )
            }

            match sess.run_codegen(hir, main_file) {
                Ok(_) => {}
                Err(e) => {
                    comp.dcx()
                        .emit_inner(e.to_diagnostic(), &SimpleFiles::from_files(files()));
                }
            }

            Ok(())
        },
        "to execute project",
    )?;

    Ok(())
}

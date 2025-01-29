use crate::{
    cli::{
        RunArgs, codegen_debug, debug_mode, dynamic_lib_mode, min_size_rel_mode, no_write, opt,
        rel_with_deb_info_mode, release_mode, static_lib_mode,
    },
    plural::plural,
};
use anstream::ColorChoice;
use anyhow::{Result, bail};
use brim::{
    Shell,
    compiler::CompilerContext,
    create_file_parent_dirs,
    files::{SimpleFiles, files},
    resolver::Resolver,
    session::Session,
    toml::ProjectType,
};
use brim_cpp_compiler::{CppBuild, compiler::CompilerKind};
use brim_parser::parser::Parser;
use clap::{ArgAction, ArgMatches, Command};
use std::{collections::HashSet, process};

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
        .arg(no_write())
        .arg(codegen_debug())
}

pub fn run_command<'a>(
    sess: &mut Session,
    comp: &'a mut CompilerContext<'a>,
    c_choice: ColorChoice,
    args: RunArgs,
) -> Result<()> {
    let build_dir = sess.build_dir().clone();
    let project_type = sess.project_type();
    let lib_type = sess.lib_type();
    let project_name = sess.config.project.name.clone();
    let shell = &mut Shell::new(c_choice);
    let opt_level = sess.config.build.level.clone();

    sess.measure_time(
        |sess| {
            sess.assert_type(
                ProjectType::Bin,
                "Can only use `run` command on binary projects",
            )?;

            let main_file = sess.main_file()?;

            shell.status(
                "Compiling",
                format!("{} in {} mode", project_name, opt_level),
            )?;

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
                Ok(code) => {
                    let file = build_dir.join("codegen").join("main.cpp");

                    create_file_parent_dirs(&file)?;

                    if !args.no_write && !file.exists() {
                        bail!("Found `no-write` flag but file doesn't exist. Try to run without `no-write` flag first");
                    }

                    if !args.no_write {
                        std::fs::write(&file, code)?;
                    }

                    let build_process = &mut CppBuild::new(
                        Some(CompilerKind::Clang),
                        project_type,
                        build_dir,
                        lib_type,
                    )?;
                    build_process.set_opt_level(opt_level).disable_warnings();
                    build_process.add_source(file);

                    let exe_path = build_process.compile(project_name, shell)?;

                    let args: Vec<String> = args.exec_args;

                    let mut command = process::Command::new(&exe_path);
                    command.args(&args);

                    shell.status(
                        "Running",
                        format!(
                            "`{}{}{}`",
                            &exe_path.to_string_lossy(),
                            if args.is_empty() { "" } else { " " },
                            &args.join(" ")
                        ),
                    )?;

                    command.status()?;
                }
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

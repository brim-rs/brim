use std::process;
use crate::{
    cli::{
        debug_mode, dynamic_lib_mode, min_size_rel_mode, opt, rel_with_deb_info_mode, release_mode,
        static_lib_mode,
    },
    compilation::{code_gen::CodeGen, imports::UnitLoader, unit::CompilationUnit},
    context::GlobalContext,
    error::{
        diagnostic::{Diagnostic, Diagnostics, Level},
        BrimError,
    },
};
use anyhow::Result;
use brim_config::ProjectType;
use brim_cpp_compiler::CppBuild;
use brim_shell::Shell;
use clap::{ArgAction, ArgMatches, Command};
use std::sync::Arc;
use tracing::debug;
use brim_cpp_compiler::compiler::CompilerKind;

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
                .trailing_var_arg(true)
        )
}

pub fn run_command(ctx: &mut GlobalContext, args: &ArgMatches, shell: &mut Shell) -> Result<()> {
    let start = ctx.start;
    let time = args.get_flag("time");

    if ctx.project_type()? == ProjectType::Lib {
        shell.error("Cannot run a library project")?;
        return Ok(());
    }

    let loader = &mut UnitLoader::new(ctx.cwd.clone());

    let lib_type = &ctx.config.build.lib_type;
    let build_process = &mut CppBuild::new(
        // msvc still doesn't support compound statements expressions
        Some(CompilerKind::Clang),
        ctx.project_type()?,
        ctx.build_dir()?,
        lib_type.clone(),
    )?;
    let mut unit = CompilationUnit::new(ctx.get_main_file()?)?;
    let diags = &mut Diagnostics::new();

    let source = Arc::new(unit.source.clone());

    match unit.compile(loader, diags) {
        Ok(_) => {
            if diags.has_errors() {
                debug!("Found error diagnostics. Skipping code generation");
                diags.print_diagnostics();
                return Ok(());
            }

            let build_type = &ctx.config.build.level;
            let codegen = &mut CodeGen::new(&mut unit, loader, build_type.clone(), true)?;

            shell.status(
                "Compiling",
                format!("{} in {} mode", ctx.config.project.name, build_type),
            )?;

            codegen.generate_and_write(ctx, build_process)?;
            let final_path = build_process.compile(&ctx.config.project.name, shell)?;

            let args: Vec<String> = args
                .get_many::<String>("args")
                .map(|vals| vals.map(|s| s.to_string()).collect())
                .unwrap_or_default();

            let mut command = process::Command::new(&final_path);
            command.args(&args);

            shell.status("Running", format!("`{}{}{}`", &final_path
                .to_string_lossy(), if args.len() > 0 { " " } else { "" }, &args.iter().map(|s| s.as_str()).collect::<Vec<&str>>().join(" ")
            ))?;

            command.status()?;
        }
        Err(e) => {
            if let Some(err) = e.downcast_ref::<BrimError>() {
                let diagnostic = err.to_diagnostic();
                diags.new_diagnostic(diagnostic, source);
            } else {
                diags.new_diagnostic(
                    Diagnostic {
                        text: format!("{}", e),
                        level: Level::Error,
                        labels: vec![],
                        hint: vec![],
                        code: None,
                    },
                    source,
                );
            }
            diags.print_diagnostics();
        }
    }

    if time {
        let elapsed = start.elapsed();
        shell.status("Time taken", &format!("{:?}", elapsed))?;
    }

    Ok(())
}

pub fn compile_unit(
    unit: &mut CompilationUnit,
    diags: &mut Diagnostics,
    loader: &mut UnitLoader,
) -> Result<()> {
    unit.compile(loader, diags)?;

    Ok(())
}

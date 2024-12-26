use std::fmt::format;
use crate::{
    cli::opt,
    compilation::{imports::UnitLoader, unit::CompilationUnit},
    context::{GlobalContext},
    error::{
        diagnostic::{Diagnostic, Diagnostics, Level},
        BrimError,
    },
};
use anyhow::Result;
use clap::{ArgAction, ArgMatches, Command};
use std::sync::Arc;
use tracing::debug;
use brim_config::{LibType, OptLevel, ProjectType};
use brim_cpp_compiler::CppBuild;
use crate::cli::{debug_mode, release_mode};
use crate::compilation::code_gen::CodeGen;

pub fn run_cmd() -> Command {
    Command::new("run").about("Run a project").arg(
        opt("time", "Prints the time taken to run the project")
            .short('t')
            .action(ArgAction::SetTrue),
    )
        .arg(release_mode())
        .arg(debug_mode())
}

pub fn run_command(ctx: &mut GlobalContext, args: &ArgMatches) -> Result<()> {
    let start = ctx.start;
    let time = args.get_flag("time");

    // if ctx.project_type()? == ProjectType::Lib {
    //     ctx.brim_shell.error("Cannot run a library project")?;
    //     return Ok(());
    // }

    let loader = &mut UnitLoader::new(ctx.cwd.clone());

    let lib_type = ctx.config.build.as_ref().and_then(|b| b.lib_type.clone()).unwrap_or(LibType::Static);
    let build_process = &mut CppBuild::new(None, ctx.project_type()?, ctx.build_dir()?, lib_type)?;
    let mut unit = CompilationUnit::new(ctx.get_main_file()?)?;
    let diags = &mut Diagnostics::new();

    let source = Arc::new(unit.source.clone());

    match unit.compile(loader, diags) {
        Ok(_) => {
            if diags.diagnostics.len() > 0 {
                debug!("Found diagnostics. Skipping code generation");
                diags.print_diagnostics();
                return Ok(());
            }

            // TODO: move that to config
            // let build_type = resolve_build_type(&ctx.config, args)?;
            // debug!("Build type: {:?}", build_type);
            let build_type = ctx.config.build.as_ref().and_then(|b| b.level.clone()).unwrap_or(OptLevel::Debug);
            let codegen = &mut CodeGen::new(&mut unit, loader, build_type.clone(), true)?;

            ctx.shell.status("Compiling", format!("{} in {} mode", ctx.config.project.name, build_type))?;

            codegen.generate_and_write(ctx, build_process)?;

            build_process.compile(
                &ctx.config.project.name, &mut ctx.shell,
            )?;
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
        ctx.shell.status("Time taken", &format!("{:?}", elapsed))?;
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

use crate::{
    cli::opt,
    compilation::{imports::UnitLoader, unit::CompilationUnit},
    context::{GlobalContext, ProjectType},
    error::{
        diagnostic::{Diagnostic, Diagnostics, Level},
        BrimError,
    },
};
use anyhow::Result;
use clap::{ArgAction, ArgMatches, Command};
use std::sync::Arc;

pub fn run_cmd() -> Command {
    Command::new("run").about("Run a project").arg(
        opt("time", "Prints the time taken to run the project")
            .short('t')
            .action(ArgAction::SetTrue),
    )
}

pub fn run_command(ctx: &mut GlobalContext, args: &ArgMatches) -> Result<()> {
    let start = ctx.start;
    let time = args.get_flag("time");

    if ctx.project_type()? == ProjectType::Lib {
        ctx.shell.error("Cannot run a library project")?;
        return Ok(());
    }

    let loader = &mut UnitLoader::new(ctx.cwd.clone());
    let mut unit = CompilationUnit::new(ctx.get_main_file()?)?;
    let diags = &mut Diagnostics::new();

    let source = Arc::new(unit.source.clone());

    match unit.compile(loader, diags) {
        Ok(_) => {
            if diags.diagnostics.len() > 0 {
                diags.print_diagnostics();
                return Ok(());
            }

            // run execution
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

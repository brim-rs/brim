use std::sync::Arc;
use clap::{ArgAction, ArgMatches, Command};
use crate::context::{GlobalContext, ProjectType};
use anyhow::Result;
use crate::cli::opt;
use crate::compilation::imports::UnitLoader;
use crate::compilation::unit::CompilationUnit;
use crate::error::BrimError;
use crate::error::diagnostic::{Diagnostic, Diagnostics, Level};

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
    let mut diags = &mut Diagnostics::new();

    compile_unit(&mut unit, &mut diags, loader)?;

    if time {
        let elapsed = start.elapsed();
        ctx.shell.status("Time taken", &format!("{:?}", elapsed))?;
    }

    Ok(())
}

pub fn compile_unit(unit: &mut CompilationUnit, diags: &mut Diagnostics, loader: &mut UnitLoader) -> Result<()> {
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
                diags.new_diagnostic(Diagnostic {
                    text: format!("{}", e),
                    level: Level::Error,
                    labels: vec![],
                    hint: vec![],
                    code: None,
                }, source);
            }
            diags.print_diagnostics();
        }
    }

    Ok(())
}
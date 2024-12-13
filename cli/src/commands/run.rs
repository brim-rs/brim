use std::sync::Arc;
use clap::{ArgAction, ArgMatches, Command};
use crate::context::GlobalContext;
use anyhow::Result;
use crate::cli::opt;
use crate::compilation::unit::CompilationUnit;
use crate::error::BrimError;

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

    // TODO: update to detect using a brim.toml file
    let mut unit = CompilationUnit::new(ctx.cwd.join("main.brim"), ctx)?;

    match unit.compile() {
        Ok(_) => {
            ctx.print_diagnostics();
        }
        Err(e) => {
            if let Some(err) = e.downcast_ref::<BrimError>() {
                let diagnostic = err.to_diagnostic();
                let source = Arc::new(unit.source.clone());

                ctx.new_diagnostic(diagnostic, source.clone());
            } else {
                ctx.shell.error(&format!("{}", e))?;
            }

            ctx.print_diagnostics();

            return Ok(());
        }
    }

    if time {
        let elapsed = start.elapsed();
        ctx.shell.status("Time taken", &format!("{:?}", elapsed))?;
    }

    Ok(())
}

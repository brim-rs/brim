use crate::commands::run::run_cmd;
use brim_shell::styles::*;
use clap::{builder::Styles, Arg, ArgAction, Command};

pub fn opt(name: &'static str, help: &'static str) -> Arg {
    Arg::new(name).long(name).help(help).action(ArgAction::Set)
}

pub fn positional(name: &'static str, help: &'static str) -> Arg {
    Arg::new(name).help(help).index(1)
}

pub fn build_type() -> Arg {
    Arg::new("build-type")
        .long("build-type")
        .help("The type of build to run")
        .default_value("debug")
}

pub fn cli() -> Command {
    let styles = {
        Styles::styled()
            .header(HEADER)
            .usage(USAGE)
            .literal(LITERAL)
            .placeholder(PLACEHOLDER)
            .error(ERROR)
            .valid(VALID)
            .invalid(INVALID)
    };

    Command::new("brim")
        .allow_external_subcommands(true)
        .styles(styles)
        .arg(
            opt("verbose", "Use verbose output")
                .short('v')
                .action(ArgAction::SetTrue)
                .global(true),
        )
        .arg(
            opt("no-color", "Disable colored output")
                .long("no-color")
                .action(ArgAction::SetTrue)
                .global(true),
        )
        .subcommand(run_cmd())
}

use crate::commands::run::run_cmd;
use brim_shell::styles::*;
use clap::{builder::Styles, Arg, ArgAction, Command};

pub fn opt(name: &'static str, help: &'static str) -> Arg {
    Arg::new(name).long(name).help(help).action(ArgAction::Set)
}

pub fn positional(name: &'static str, help: &'static str) -> Arg {
    Arg::new(name).help(help).index(1)
}

pub const COMPILATION_HEADING: &str = "Compilation options";

pub fn release_mode() -> Arg {
    opt("release", "Build in release mode")
        .action(ArgAction::SetTrue)
        .short('r')
        .conflicts_with("debug")
        .help_heading(COMPILATION_HEADING)
}

pub fn debug_mode() -> Arg {
    opt("debug", "Build in debug mode")
        .action(ArgAction::SetTrue)
        .short('d')
        .conflicts_with("release")
        .help_heading(COMPILATION_HEADING)
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

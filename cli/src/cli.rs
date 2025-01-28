use crate::commands::run::run_cmd;
use brim::styles::{ERROR, HEADER, INVALID, LITERAL, PLACEHOLDER, USAGE, VALID};
use clap::{Arg, ArgAction, Command, builder::Styles, ArgMatches};

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
        .conflicts_with_all(&["debug", "min-size-rel", "rel-with-deb-info"])
        .help_heading(COMPILATION_HEADING)
}

pub fn debug_mode() -> Arg {
    opt("debug", "Build in debug mode")
        .action(ArgAction::SetTrue)
        .short('d')
        .conflicts_with_all(&["release", "min-size-rel", "rel-with-deb-info"])
        .help_heading(COMPILATION_HEADING)
}

pub fn min_size_rel_mode() -> Arg {
    opt("min-size-rel", "Build in min-size-rel mode")
        .action(ArgAction::SetTrue)
        .short('m')
        .conflicts_with_all(&["release", "debug", "rel-with-deb-info"])
        .help_heading(COMPILATION_HEADING)
}

pub fn rel_with_deb_info_mode() -> Arg {
    opt("rel-with-deb-info", "Build in rel-with-deb-info mode")
        .action(ArgAction::SetTrue)
        .short('i')
        .conflicts_with_all(&["release", "debug", "min-size-rel"])
        .help_heading(COMPILATION_HEADING)
}

pub fn dynamic_lib_mode() -> Arg {
    opt("dynamic", "Build a dynamic library")
        .action(ArgAction::SetTrue)
        .short('l')
        .conflicts_with("static")
        .help_heading(COMPILATION_HEADING)
}

pub fn static_lib_mode() -> Arg {
    opt("static", "Build a static library")
        .action(ArgAction::SetTrue)
        .short('s')
        .conflicts_with("dynamic")
        .help_heading(COMPILATION_HEADING)
}

pub fn no_write() -> Arg {
    opt("no-write", "Do not write the output to a file")
        .action(ArgAction::SetTrue)
        .short('w')
}

pub fn codegen_debug() -> Arg {
    opt(
        "codegen-debug",
        "Displays generated c++ code in the terminal",
    )
        .short('c')
        .action(ArgAction::SetTrue)
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
        .arg(
            opt("time", "Prints the time taken to run the project")
                .short('t')
                .action(ArgAction::SetTrue)
                .global(true),
        )
        .subcommand(run_cmd())
}

#[derive(Debug)]
pub struct RunArgs {
    pub no_write: bool,
    pub codegen_debug: bool,
    pub exec_args: Vec<String>
}

impl RunArgs {
    pub fn from_args(args: &ArgMatches) -> Self {
        Self {
            no_write: args.get_flag("no-write"),
            codegen_debug: args.get_flag("codegen-debug"),
            exec_args: args
                .get_many::<String>("args")
                .unwrap_or_default()
                .map(|s| s.to_string())
                .collect()
        }
    }
}

use clap::ArgMatches;

#[derive(Debug, Clone)]
pub struct RunArgs {
    pub no_write: bool,
    pub codegen_debug: bool,
    pub exec_args: Vec<String>,
    pub time: bool,
}

impl RunArgs {
    pub fn from_args(args: &ArgMatches) -> Self {
        Self {
            no_write: args.get_flag("no-write"),
            codegen_debug: args.get_flag("codegen-debug"),
            exec_args: args
                .get_many::<String>("args")
                .unwrap_or_default()
                .map(std::string::ToString::to_string)
                .collect(),
            time: args.get_flag("time"),
        }
    }
}

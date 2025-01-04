use std::path::PathBuf;
use anyhow::Result;
use brim::compiler::CompilerContext;
use brim::files::{files, SimpleFiles};
use brim_parser::parser_from_simple_file;

pub fn run_tests(exec: PathBuf) -> Result<()> {
    for file in files() {
        let ctx = &mut CompilerContext::new();
        let mut parser = parser_from_simple_file(&file)?;
        parser.keep_comments = true;
        parser.parse_barrel(ctx)?;
    }

    Ok(())
}
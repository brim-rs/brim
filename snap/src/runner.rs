use std::path::PathBuf;
use anyhow::Result;
use brim::compiler::CompilerContext;
use brim::files::SimpleFiles;
use brim_parser::parser_from_simple_file;

pub fn run_tests(exec: PathBuf, mut files: &mut SimpleFiles) -> Result<()> {
    for file in files.clone() {
        let ctx = &mut CompilerContext::new(&mut files);
        let mut parser = parser_from_simple_file(&file)?;
        parser.parse_barrel(ctx)?;
    }

    Ok(())
}
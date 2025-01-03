mod lexer;
mod parser;

use crate::parser::Parser;
use anyhow::Result;
use brim::files::SimpleFile;
use tracing::debug;

pub fn parser_from_simple_file(file: &SimpleFile) -> Result<Parser> {
    let parser = Parser::new(file);
    debug!("Created new parser for file: {}", file.name().display());

    Ok(parser)
}

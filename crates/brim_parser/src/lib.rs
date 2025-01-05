pub mod lexer;
pub mod parser;

use crate::parser::Parser;
use anyhow::Result;
use brim::files::SimpleFile;
use tracing::debug;
use brim_symbols_macro::generate_symbols;

pub fn parser_from_simple_file(file: &SimpleFile) -> Result<Parser> {
    let parser = Parser::new(file);
    debug!("Created new parser for file: {}", file.name().display());

    Ok(parser)
}

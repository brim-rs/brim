mod parser;
mod lexer;

use anyhow::Result;
use tracing::debug;
use brim::files::SimpleFile;
use brim::session::Session;
use crate::parser::Parser;

pub fn parser_from_simple_file<'a>(sess: &'a Session<'a>, file: &'a SimpleFile) -> Result<Parser<'a>> {
    let parser = Parser::new(sess, file);
    debug!("Created new parser for file: {}", file.name().display());
    
    Ok(parser)
}
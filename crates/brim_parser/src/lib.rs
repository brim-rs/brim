mod parser;

use anyhow::Result;
use brim::files::SimpleFile;
use brim::session::Session;
use crate::parser::Parser;

pub fn parser_from_simple_file<'a>(sess: &'a Session<'a>, file: &'a SimpleFile) -> Result<Parser<'a>> {
    let mut parser = Parser::new(sess, file);
    Ok(parser)
}
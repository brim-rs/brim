use std::fs::File;
use std::path::PathBuf;
use anyhow::bail;
use crate::context::GlobalContext;
use crate::error::BrimError::CliError;
use crate::lexer::Lexer;
use crate::lexer::source::Source;
use crate::Result;

#[derive(Debug)]
pub struct CompilationUnit<'a> {
    pub source: Source,
    pub context: &'a mut GlobalContext,
    pub lexer: Lexer,
}

impl<'a> CompilationUnit<'a> {
    pub fn new(path: PathBuf, context: &'a mut GlobalContext) -> Result<Self> {
        let source = Source::from_reader(path.clone(), File::open(path)?)?;
        Ok(Self {
            source: source.clone(),
            context,
            lexer: Lexer::new(source),
        })
    }

    pub fn compile(&mut self) -> Result<()> {
        self.lexer.lex()?;

        Ok(())
    }
}
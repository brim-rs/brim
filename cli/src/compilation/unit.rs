use std::fs::File;
use std::path::PathBuf;
use anyhow::bail;
use crate::ast::Ast;
use crate::context::GlobalContext;
use crate::error::BrimError::CliError;
use crate::lexer::Lexer;
use crate::lexer::source::Source;
use crate::parser::Parser;
use crate::Result;

#[derive(Debug)]
pub struct CompilationUnit<'a> {
    pub source: Source,
    pub context: &'a mut GlobalContext,
    pub lexer: Lexer,
    pub parser: Parser<'a>,
}

impl<'a> CompilationUnit<'a> {
    pub fn new(path: PathBuf, context: &'a mut GlobalContext) -> Result<Self> {
        let source = Source::from_reader(path.clone(), File::open(path)?)?;
        Ok(Self {
            source: source.clone(),
            context,
            lexer: Lexer::new(source),
            parser: Parser::new(vec![], Box::leak(Box::new(Ast::new()))),
        })
    }

    pub fn compile(&mut self) -> Result<()> {
        self.lexer.lex()?;
        self.parser.tokens = self.lexer.tokens.clone();

        self.parser.parse()?;


        Ok(())
    }
}
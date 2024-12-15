use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf;
use crate::ast::Ast;
use crate::compilation::imports::UnitLoader;
use crate::compilation::items::{UnitItem, UnitItemKind};
use crate::compilation::passes::resolver::Resolver;
use crate::error::diagnostic::Diagnostics;
use crate::lexer::Lexer;
use crate::lexer::source::Source;
use crate::parser::Parser;
use crate::Result;


#[derive(Debug, Clone)]
pub struct CompilationUnit {
    pub source: Source,
    pub lexer: Lexer,
    pub parser: Parser,
    // Units that are used in imports etc
    pub unit_items: HashMap<String, UnitItem>,
}

impl CompilationUnit {
    pub fn new(path: PathBuf) -> Result<Self> {
        let source = Source::from_reader(path.clone(), File::open(path)?)?;
        Ok(Self {
            source: source.clone(),
            lexer: Lexer::new(source),
            parser: Parser::new(vec![], Ast::new()),
            unit_items: HashMap::new(),
        })
    }

    pub fn new_item(&mut self, name: String, kind: UnitItemKind, unit: String, public: bool) {
        self.unit_items.insert(name, UnitItem {
            kind,
            imported: false,
            unit,
            public,
        });
    }

    pub fn new_imported_item(&mut self, name: String, kind: UnitItemKind, unit: String, public: bool) {
        self.unit_items.insert(name, UnitItem {
            kind,
            imported: true,
            unit,
            public,
        });
    }

    pub fn compile(&mut self, loader: &mut UnitLoader, diags: &mut Diagnostics) -> Result<()> {
        self.lexer.lex()?;
        self.parser.tokens = self.lexer.tokens.clone();

        self.parser.parse()?;

        let mut resolver = Resolver {
            unit: self,
            loader,
            diags,
        };

        resolver.run()?;

        Ok(())
    }

    pub fn ast(&self) -> &Ast {
        &self.parser.ast
    }
}
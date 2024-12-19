use crate::{
    ast::Ast,
    compilation::{
        imports::UnitLoader,
        items::{UnitItem, UnitItemKind},
        passes::{resolver::Resolver, type_checker::pass::TypeChecker},
    },
    error::diagnostic::Diagnostics,
    lexer::{source::Source, Lexer},
    parser::Parser,
    Result,
};
use std::{collections::HashMap, fs::File, path::PathBuf};

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

    pub fn path(&self) -> String {
        self.source.path.display().to_string()
    }

    pub fn new_item(&mut self, name: String, kind: UnitItemKind, unit: String, public: bool) {
        self.unit_items.insert(
            name,
            UnitItem {
                kind,
                imported: false,
                unit,
                public,
            },
        );
    }

    pub fn new_imported_item(
        &mut self,
        name: String,
        kind: UnitItemKind,
        unit: String,
        public: bool,
    ) {
        self.unit_items.insert(
            name,
            UnitItem {
                kind,
                imported: true,
                unit,
                public,
            },
        );
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

        let mut type_checker = TypeChecker {
            scopes: vec![HashMap::new()],
            unit: self,
            loader,
            diags,
        };

        type_checker.run()?;

        Ok(())
    }

    pub fn ast(&self) -> &Ast {
        &self.parser.ast
    }
}

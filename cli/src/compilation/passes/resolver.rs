use std::sync::Arc;
use anstream::ColorChoice;
use crate::compilation::unit::{CompilationUnit, UnitItemKind};
use anyhow::Result;
use colored::Colorize;
use crate::ast::{Ast, StmtId};
use crate::ast::statements::{Function, Stmt, StmtKind};
use crate::commands::run::compile_unit;
use crate::compilation::imports::{UnitLoader};
use crate::compilation::passes::Pass;
use crate::error::diagnostic::{Diagnostic, Diagnostics, Level};

#[derive(Debug)]
pub struct Resolver<'a> {
    pub unit: &'a mut CompilationUnit,
    pub loader: &'a mut UnitLoader,
    pub diags: &'a mut Diagnostics,
}

impl<'a> Resolver<'a> {
    pub fn run(&mut self) -> Result<()> {
        let unit = &mut self.unit.clone();
        for item in unit.parser.ast.top_level_items.cloned_indices() {
            self.visit_item(unit, item)?;
        }

        Ok(())
    }
}

impl<'a> Pass for Resolver<'a> {
    fn do_visit_statement(&mut self, statement: StmtId) -> Result<()> {
        let statement = self.unit.ast().query_stmt(statement).clone();

        match statement.kind {
            StmtKind::Struct(struct_stmt) => {
                self.unit.new_item(struct_stmt.name.literal(), UnitItemKind::Struct(struct_stmt.clone()), self.unit.source.path.display().to_string());
            }
            StmtKind::Fn(fn_stmt) => {
                self.unit.new_item(fn_stmt.name.clone(), UnitItemKind::Function(fn_stmt), self.unit.source.path.display().to_string());
            }
            StmtKind::TraitDef(trait_stmt) => {
                self.unit.new_item(trait_stmt.name.literal(), UnitItemKind::Trait(trait_stmt.clone()), self.unit.source.path.display().to_string());
            }
            StmtKind::Use(use_stmt) => {
                let (cache_key, mut unit) = self.loader.load_unit(&use_stmt.from.literal(), self.unit)?;

                compile_unit(&mut unit, self.diags, self.loader)?;
                self.loader.units.insert(cache_key, unit.clone());

                for item in use_stmt.items {
                    let name = item.literal();

                    let kind = match unit.unit_items.get(&name) {
                        Some(unit_item) => {
                            if unit_item.imported {
                                self.diags.new_diagnostic(Diagnostic {
                                    text: format!("Tried to import item `{}` from file {}, but it comes from file {}", name, unit.source.path.display(), unit_item.unit),
                                    level: Level::Error,
                                    labels: vec![
                                        (item.span.clone(), None),
                                    ],
                                    hint: vec![],
                                    code: None,
                                }, Arc::new(self.unit.source.clone()));
                                continue;
                            }

                            unit_item.kind.clone()
                        }
                        None => {
                            self.diags.new_diagnostic(Diagnostic {
                                text: format!("Item `{}` not found in file {}", name, unit.source.path.display()),
                                level: Level::Error,
                                labels: vec![
                                    (item.span.clone(), None),
                                ],
                                hint: vec![],
                                code: None,
                            }, Arc::new(self.unit.source.clone()));
                            continue;
                        }
                    };

                    self.unit.new_imported_item(name, kind, unit.source.path.display().to_string());
                }
            }
            _ => {}
        }

        Ok(())
    }
}
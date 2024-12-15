use std::sync::Arc;
use anyhow::Result;
use crate::ast::{Ast, StmtId};
use crate::ast::statements::{Function, Stmt, StmtKind};
use crate::commands::run::compile_unit;
use crate::compilation::imports::{UnitLoader};
use crate::compilation::items::UnitItemKind;
use crate::compilation::passes::Pass;
use crate::compilation::unit::CompilationUnit;
use crate::error::diagnostic::{Diagnostic, Diagnostics, Level};
use crate::path::strip_base;

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
                self.unit.new_item(struct_stmt.name.literal(), UnitItemKind::Struct(struct_stmt.clone()), self.unit.source.path.display().to_string(), struct_stmt.public);
            }
            StmtKind::Fn(fn_stmt) => {
                self.unit.new_item(fn_stmt.name.clone(), UnitItemKind::Function(fn_stmt.clone()), self.unit.source.path.display().to_string(), fn_stmt.public);
            }
            StmtKind::TraitDef(trait_stmt) => {
                self.unit.new_item(trait_stmt.name.literal(), UnitItemKind::Trait(trait_stmt.clone()), self.unit.source.path.display().to_string(), trait_stmt.public);
            }
            StmtKind::Const(const_stmt) => {
                self.unit.new_item(const_stmt.ident.literal(), UnitItemKind::Const(const_stmt.clone()), self.unit.source.path.display().to_string(), const_stmt.public);
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
                                let original_path = strip_base(unit_item.unit.clone().into(), self.loader.cwd.clone());

                                self.diags.new_diagnostic(Diagnostic {
                                    text:
                                    format!("Tried to import {} `{}` that comes from `{}` and not from {}",
                                            unit_item.kind,
                                            name,
                                            original_path.display(),
                                            strip_base(unit.source.path.clone(), self.loader.cwd.clone()).display()
                                    ),
                                    level: Level::Error,
                                    labels: vec![
                                        (item.span.clone(), None),
                                    ],
                                    hint: vec![
                                        format!("Remove the `use` statement and import the item from {} directly", original_path.display()),
                                    ],
                                    code: None,
                                }, Arc::new(self.unit.source.clone()));
                                continue;
                            }

                            if !unit_item.public {
                                self.diags.new_diagnostic(Diagnostic {
                                    text: format!("item `{}` isn't public", name),
                                    level: Level::Error,
                                    labels: vec![
                                        (item.span, Some(format!("private {}", unit_item.kind))),
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

                    self.unit.new_imported_item(name, kind, unit.source.path.display().to_string(), true);
                }
            }
            _ => {}
        }

        Ok(())
    }
}
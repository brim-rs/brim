use crate::{
    ast::{
        statements::{StmtKind, StoredStructImpl, StoredTraitImpl},
        GetSpan, StmtId,
    },
    commands::run::compile_unit,
    compilation::{
        imports::{remove_surrounding_quotes, UnitLoader},
        items::UnitItemKind,
        passes::Pass,
        unit::CompilationUnit,
    },
    error::{
        diagnostic::{Diagnostic, Diagnostics, Level},
        span::TextSpan,
    },
    path::strip_base,
};
use anyhow::Result;
use std::sync::Arc;

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
                self.unit.new_item(
                    struct_stmt.name.literal(),
                    UnitItemKind::Struct(struct_stmt.clone()),
                    self.unit.path().to_string(),
                    struct_stmt.public,
                );
            }
            StmtKind::TraitImpl(trait_impl) => {
                let trait_name = trait_impl.trait_name.literal();

                let trait_item = match self.unit.unit_items.get(&trait_name) {
                    Some(item) => item,
                    None => {
                        self.diags.new_diagnostic(
                            Diagnostic::error(
                                format!(
                                    "Attempted to implement trait `{}` that doesn't exist",
                                    trait_name
                                ),
                                vec![(trait_impl.trait_name.span.clone(), None)],
                                vec![],
                            ),
                            Arc::new(self.unit.source.clone()),
                        );
                        return Ok(());
                    }
                };

                let trait_def = match &trait_item.kind {
                    UnitItemKind::Trait(trait_def) => trait_def,
                    _ => {
                        self.diags.new_diagnostic(
                            Diagnostic::error(
                                format!(
                                    "Attempted to implement item `{}` for `{}` that isn't a trait",
                                    trait_name, trait_item.kind
                                ),
                                vec![(trait_impl.trait_name.span.clone(), None)],
                                vec![],
                            ),
                            Arc::new(self.unit.source.clone()),
                        );

                        return Ok(());
                    }
                };

                let struct_name = trait_impl.struct_name.literal();

                let struct_item = match self.unit.unit_items.get(&struct_name) {
                    Some(item) => item,
                    None => {
                        self.diags.new_diagnostic(
                            Diagnostic::error(format!("Attempted to implement trait `{}` for struct `{}` that doesn't exist", trait_name, struct_name), vec![
                                (trait_impl.struct_name.span.clone(), None)
                            ], vec![]), Arc::new(self.unit.source.clone()));

                        return Ok(());
                    }
                };

                let struct_def = &mut match struct_item.kind.clone() {
                    UnitItemKind::Struct(struct_def) => struct_def,
                    _ => {
                        self.diags.new_diagnostic(Diagnostic::error(format!("Attempted to implement trait `{}` for item `{}` that isn't a struct", trait_name, struct_name), vec![
                            (trait_impl.struct_name.span.clone(), None)
                        ], vec![]), Arc::new(self.unit.source.clone()));

                        return Ok(());
                    }
                };

                if struct_def.trait_impls.iter().any(|t| {
                    t.trait_impl.trait_name.literal() == trait_name && trait_item.unit == t.unit
                }) {
                    self.diags.new_diagnostic(
                        Diagnostic::error(
                            format!(
                                "Struct `{}` already implements trait `{}`",
                                struct_name, trait_name
                            ),
                            vec![(trait_impl.struct_name.span.clone(), None)],
                            vec![],
                        ),
                        Arc::new(self.unit.source.clone()),
                    );

                    return Ok(());
                }

                let unit = &mut self.unit.clone();
                let defining_unit = self.loader.load_unit(&trait_item.unit, unit)?.1;

                for method in &trait_impl.methods {
                    let method = defining_unit.parser.ast.query_stmt(*method).clone();
                    let method = method.as_function().clone();
                    let method_name = method.name.literal();

                    let method_item = match trait_def.methods.iter().find(|m| {
                        let m = defining_unit.parser.ast.query_stmt(**m).clone();

                        m.as_function().name.literal() == method_name
                    }) {
                        Some(item) => item,
                        None => {
                            self.diags.new_diagnostic(Diagnostic::error(
                                format!("Tried to implement method `{}` that isn't defined in trait `{}`", method_name, trait_name),
                                vec![
                                    (method.name.span, None),
                                ],
                                vec![
                                    "If you want to implement it as a struct method, use `impl` instead".to_string(),
                                ],
                            ), Arc::new(self.unit.source.clone()));

                            continue;
                        }
                    };

                    let method_def = defining_unit
                        .parser
                        .ast
                        .query_stmt(*method_item)
                        .clone()
                        .as_function()
                        .clone();
                    let missing_params = method_def
                        .params
                        .iter()
                        .filter(|param| {
                            !method
                                .params
                                .iter()
                                .any(|p| p.ident.literal() == param.ident.literal())
                        })
                        .map(|param| param.ident.literal())
                        .collect::<Vec<String>>();

                    if !missing_params.is_empty() {
                        self.diags.new_diagnostic(
                            Diagnostic::error(
                                format!(
                                    "Method `{}` is missing parameters: {}",
                                    method_name,
                                    missing_params.join(", ")
                                ),
                                vec![(method.name.span.clone(), None)],
                                vec![],
                            ),
                            Arc::new(self.unit.source.clone()),
                        )
                    }

                    for param in method_def.params.iter().zip(method.params.iter()) {
                        let (defining_param, param) = param;

                        if defining_param.type_annotation != param.type_annotation {
                            // TODO: when one of the params is from different file, the span will show wrong code
                            let vec = vec![
                                (
                                    defining_param.ident.span.clone(),
                                    Some("trait definition".to_string()),
                                ),
                                (param.ident.span.clone(), Some("implementation".to_string())),
                            ];

                            self.diags.new_diagnostic(Diagnostic::error(
                                format!("Method `{}` parameter `{}` has a different type from the trait definition", method_name, defining_param.ident.literal()),
                                vec,
                                vec![],
                            ), Arc::new(self.unit.source.clone()));
                        }
                    }

                    if method_def.return_type != method.return_type {
                        let mut spans = vec![method.name.span];

                        if let Some(return_type) = &method.return_type {
                            spans.push(return_type.span(&defining_unit.parser.ast).clone());
                        }

                        self.diags.new_diagnostic(Diagnostic::error(
                            format!("Method `{}` has a different return type from the trait definition", method_name),
                            vec![
                                (TextSpan::combine(spans).unwrap(), None),
                            ],
                            vec![],
                        ), Arc::new(self.unit.source.clone()));
                    }
                }

                struct_def.trait_impls.push(StoredTraitImpl {
                    trait_impl: trait_impl.clone(),
                    unit: struct_item.unit.clone(),
                });

                self.unit.new_item(
                    struct_name,
                    UnitItemKind::Struct(struct_def.clone()),
                    self.unit.path().to_string(),
                    struct_item.public,
                );
            }
            StmtKind::StructImpl(struct_impl) => {
                let struct_name = struct_impl.struct_name.literal();

                let struct_item = match self.unit.unit_items.get(&struct_name) {
                    Some(item) => item,
                    None => {
                        self.diags.new_diagnostic(
                            Diagnostic {
                                text: format!(
                                    "Attempted to implement struct `{}` that doesn't exist",
                                    struct_name
                                ),
                                level: Level::Error,
                                labels: vec![(struct_impl.struct_name.span.clone(), None)],
                                hint: vec![],
                                code: None,
                            },
                            Arc::new(self.unit.source.clone()),
                        );
                        return Ok(());
                    }
                };

                let struct_def = &mut match struct_item.kind.clone() {
                    UnitItemKind::Struct(struct_def) => struct_def,
                    _ => {
                        self.diags.new_diagnostic(
                            Diagnostic {
                                text: format!(
                                    "Attempted to implement item `{}` that isn't a struct",
                                    struct_name
                                ),
                                level: Level::Error,
                                labels: vec![(struct_impl.struct_name.span.clone(), None)],
                                hint: vec![],
                                code: None,
                            },
                            Arc::new(self.unit.source.clone()),
                        );
                        return Ok(());
                    }
                };

                struct_def.impls.push(StoredStructImpl {
                    struct_impl: struct_impl.clone(),
                    unit: self.unit.path().to_string(),
                });

                self.unit.new_item(
                    struct_name,
                    UnitItemKind::Struct(struct_def.clone()),
                    self.unit.path().to_string(),
                    struct_item.public,
                );
            }
            StmtKind::Fn(fn_stmt) => {
                self.unit.new_item(
                    fn_stmt.name.literal(),
                    UnitItemKind::Function(fn_stmt.clone()),
                    self.unit.path().to_string(),
                    fn_stmt.public,
                );
            }
            StmtKind::TraitDef(trait_stmt) => {
                self.unit.new_item(
                    trait_stmt.name.literal(),
                    UnitItemKind::Trait(trait_stmt.clone()),
                    self.unit.path().to_string(),
                    trait_stmt.public,
                );
            }
            StmtKind::Const(const_stmt) => {
                self.unit.new_item(
                    const_stmt.ident.literal(),
                    UnitItemKind::Const(const_stmt.clone()),
                    self.unit.path().to_string(),
                    const_stmt.public,
                );
            }
            StmtKind::Enum(enum_stmt) => {
                for variant in enum_stmt.variants.iter() {
                    let name = variant.ident.literal();

                    for typ in variant.params.clone() {
                        let (custom, c_name) = typ.kind.is_custom();

                        if custom && !self.unit.unit_items.contains_key(c_name) {
                            if enum_stmt
                                .generics
                                .iter()
                                .any(|g| g.name.literal() == c_name)
                            {
                                continue;
                            }

                            self.diags.new_diagnostic(
                                Diagnostic::error(
                                    format!(
                                        "Type `{}` not found in file {}",
                                        c_name,
                                        self.unit.path()
                                    ),
                                    vec![(typ.span(self.unit.ast()).clone(), None)],
                                    vec![],
                                ),
                                Arc::new(self.unit.source.clone()),
                            );
                        }
                    }
                }

                self.unit.new_item(
                    enum_stmt.name.literal(),
                    UnitItemKind::Enum(enum_stmt.clone()),
                    self.unit.path().to_string(),
                    enum_stmt.public,
                );
            }
            StmtKind::Use(use_stmt) => {
                let (cache_key, mut unit) =
                    self.loader.load_unit(&use_stmt.from.literal(), self.unit)?;

                compile_unit(&mut unit, self.diags, self.loader)?;
                self.loader.units.insert(cache_key, unit.clone());
                // TODO: improve whatever this is
                self.loader.units.insert(
                    remove_surrounding_quotes(&use_stmt.from.literal()).to_string(),
                    unit.clone(),
                );

                for item in use_stmt.items {
                    let name = item.literal();

                    let kind = match unit.unit_items.get(&name) {
                        Some(unit_item) => {
                            if unit_item.imported {
                                let original_path = strip_base(
                                    &unit_item.unit.clone().into(),
                                    &self.loader.cwd.clone(),
                                );

                                self.diags.new_diagnostic(Diagnostic::error(
                                    format!("Tried to import {} `{}` that comes from `{}` and not from {}",
                                            unit_item.kind,
                                            name,
                                            original_path.display(),
                                            strip_base(&unit.source.path, &self.loader.cwd).display()
                                    ),
                                    vec![
                                        (item.span.clone(), None),
                                    ],
                                    vec![
                                        format!("Remove the `use` statement and import the item from {} directly", original_path.display()),
                                    ],
                                ), Arc::new(self.unit.source.clone()
                                ));

                                continue;
                            }

                            if !unit_item.public {
                                self.diags.new_diagnostic(
                                    Diagnostic::error(
                                        format!("item `{}` isn't public", name),
                                        vec![(
                                            item.span,
                                            Some(format!("private {}", unit_item.kind)),
                                        )],
                                        vec![],
                                    ),
                                    Arc::new(self.unit.source.clone()),
                                );

                                continue;
                            }

                            unit_item.kind.clone()
                        }
                        None => {
                            self.diags.new_diagnostic(
                                Diagnostic::error(
                                    format!("Item `{}` not found in file {}", name, unit.path()),
                                    vec![(item.span.clone(), None)],
                                    vec![],
                                ),
                                Arc::new(self.unit.source.clone()),
                            );

                            continue;
                        }
                    };

                    self.unit
                        .new_imported_item(name, kind, unit.path().to_string(), true);
                }
            }
            _ => {}
        }

        self.loader
            .units
            .insert(self.unit.path().to_string(), self.unit.clone());

        Ok(())
    }
}

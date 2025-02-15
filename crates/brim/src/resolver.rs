use crate::session::Session;
use brim_ast::item::{ItemKind, PathItemKind};
use brim_config::toml::Dependency;
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_fs::{
    loader::{BrimFileLoader, FileLoader},
    normalize_path,
};
use brim_middle::{
    GlobalSymbolId, ModuleId, barrel::Barrel, experimental::Experimental, modules::ModuleMap,
    temp_diag::TemporaryDiagnosticContext,
};
use brim_parser::parser::Parser;
use brim_span::{
    files::{add_file, get_path},
    span::Span,
};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

#[derive(Debug)]
pub struct Resolver<'a> {
    pub ctx: &'a mut TemporaryDiagnosticContext,
    pub map: ModuleMap,
    pub temp_loader: BrimFileLoader,
    pub file: usize,
    pub sess: &'a mut Session,
}

impl<'a> Resolver<'a> {
    pub fn new(ctx: &'a mut TemporaryDiagnosticContext, sess: &'a mut Session) -> Self {
        Self {
            ctx,
            map: ModuleMap::new(),
            temp_loader: BrimFileLoader,
            file: 0,
            sess,
        }
    }

    pub fn create_module_map(
        &mut self,
        barrel: &mut Barrel,
        visited: &mut HashSet<PathBuf>,
    ) -> anyhow::Result<ModuleMap> {
        let file_id = barrel.file_id.clone();
        self.file = file_id;
        let mut ref_path = get_path(file_id.clone())?;

        if visited.contains(&ref_path) {
            return Ok(self.map.clone());
        }

        visited.insert(ref_path.clone());
        self.map.insert_or_update(ref_path.clone(), barrel.clone());

        for item in barrel.items.iter_mut() {
            if let ItemKind::Use(use_stmt) = &mut item.kind {
                let path = self.build_path(&use_stmt.path, use_stmt.span.clone());

                if path.is_none() {
                    continue;
                }
                let path = path.unwrap();

                ref_path.pop();
                ref_path.push(path.clone());
                ref_path.set_extension("brim");

                if !self.temp_loader.file_exists(&ref_path) {
                    self.ctx.emit_impl(ModuleNotFound {
                        span: (use_stmt.span.clone(), file_id),
                        original_name: self.build_display_path(&use_stmt.path),
                        path: ref_path.display().to_string(),
                    });

                    continue;
                }

                let experimental = self.sess.config.experimental.clone();

                let contents = self.temp_loader.read_file(&ref_path)?;
                let file = add_file(ref_path.clone(), contents);
                let mut parser = Parser::new(file, experimental.clone());
                let mut nested_barrel = parser.parse_barrel()?;

                self.ctx.extend(parser.dcx.diags);

                self.map.add_import(
                    GlobalSymbolId {
                        mod_id: ModuleId::from_usize(file_id),
                        item_id: item.id,
                    },
                    ref_path.clone(),
                );

                self.create_module_map(&mut nested_barrel, visited)?;
            }
        }

        Ok(self.map.clone())
    }

    pub fn build_display_path(&self, path: &Vec<PathItemKind>) -> String {
        let mut display_path = String::new();

        for (i, part) in path.iter().enumerate() {
            match part {
                PathItemKind::Module(ident) => {
                    display_path.push_str(&*ident.name.as_str().expect("expected module name"))
                }
                PathItemKind::Parent => display_path.push_str("parent"),
                PathItemKind::Current => display_path.push_str("self"),
            }

            if i < path.len() - 1 {
                display_path.push_str("::");
            }
        }

        display_path
    }

    pub fn build_path(&mut self, path: &Vec<PathItemKind>, span: Span) -> Option<PathBuf> {
        let mut path_buf = PathBuf::new();

        if path[0] == PathItemKind::Current {
            for part in path {
                match part {
                    PathItemKind::Module(ident) => {
                        path_buf.push(ident.name.as_str().expect("expected module name"))
                    }
                    PathItemKind::Parent => {
                        path_buf.pop();
                    }
                    _ => {}
                }
            }
        } else {
            let dep_name = match &path[0] {
                PathItemKind::Module(ident) => ident.name.as_str().expect("expected module name"),
                _ => "".to_string(),
            };
            let dep = self.sess.config.dependencies.get(&dep_name);

            if let Some(dep) = dep {
                if dep_name == "std" {
                    path_buf.push(normalize_path(&self.sess.config.build.std, &self.sess.cwd));
                } else if dep_name == "core" {
                    path_buf.push(normalize_path(&self.sess.config.build.core, &self.sess.cwd));
                } else {
                    let dep_name = if let Some(ver) = &dep.version {
                        format!("{}-{}", dep_name, ver)
                    } else {
                        dep_name.clone()
                    };

                    path_buf.push(self.sess.dep_dir(&dep_name));

                    // TODO: add handling for paths, versions etc
                }

                if path.len() > 1 {
                    for part in path.iter().skip(1) {
                        match part {
                            PathItemKind::Module(ident) => {
                                path_buf.push(ident.name.as_str().expect("expected module name"))
                            }
                            PathItemKind::Parent => {
                                path_buf.pop();
                            }
                            _ => {}
                        }
                    }
                } else {
                    path_buf.push("main.brim");
                }
            } else {
                self.ctx.emit_impl(DependencyNotFound {
                    span: (span, self.file),
                    dep: dep_name,
                });
                return None;
            }

            println!("{:?}", path_buf);
            return Some(path_buf);
        }

        Some(path_buf)
    }
}

#[derive(Diagnostic)]
#[error("module `{original_name}` not found at path `{path}`")]
pub struct ModuleNotFound {
    #[error]
    pub span: (Span, usize),
    pub original_name: String,
    pub path: String,
}

#[derive(Diagnostic)]
#[error("dependency `{dep}` not found")]
pub struct DependencyNotFound {
    #[error]
    pub span: (Span, usize),
    pub dep: String,
}

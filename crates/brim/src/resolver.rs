use crate::session::Session;
use brim_ast::item::{Ident, ImportsKind, ItemKind, PathItemKind, Use};
use brim_config::toml::Dependency;
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_fs::{
    loader::{BrimFileLoader, FileLoader},
    normalize_path,
};
use brim_middle::{
    GlobalSymbolId, ModuleId,
    barrel::Barrel,
    experimental::Experimental,
    modules::{Module, ModuleMap},
    temp_diag::TemporaryDiagnosticContext,
};
use brim_parser::parser::Parser;
use brim_span::{
    files::{add_file, get_path},
    span::Span,
};
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
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
        let file_id = barrel.file_id;
        self.file = file_id;
        let ref_path = get_path(file_id)?;

        if visited.contains(&ref_path) {
            return Ok(self.map.clone());
        }

        visited.insert(ref_path.clone());
        self.map.insert_or_update(ref_path.clone(), barrel.clone());

        for item in barrel.items.iter_mut() {
            if let ItemKind::Use(use_stmt) = &mut item.kind {
                let path = &use_stmt.path.clone();
                let span = use_stmt.span.clone();

                let path_buf = match self.build_path(path, span.clone()) {
                    Some(p) => p,
                    None => continue,
                };

                let full_path = if path[0] == PathItemKind::Current {
                    self.sess.cwd.join(&path_buf)
                } else {
                    path_buf.clone()
                };

                if !self.temp_loader.check_if_exists(&full_path).is_ok() {
                    self.ctx.emit_impl(ModuleNotFound {
                        span: (span, self.file),
                        original_name: self.build_display_path(path),
                        path: full_path.to_string_lossy().to_string(),
                    });
                    continue;
                }

                let content = self.temp_loader.read_file(&full_path)?;
                let file = self.sess.add_file(full_path.clone(), content.clone());

                let mut parser = Parser::new(file, self.sess.config.experimental.clone());
                let mut barrel = parser.parse_barrel()?;

                self.ctx.extend(parser.dcx.diags);

                self.map.add_import(
                    GlobalSymbolId {
                        mod_id: ModuleId::from_usize(file_id),
                        item_id: item.id,
                    },
                    full_path.clone(),
                );

                self.create_module_map(&mut barrel, visited)?;
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

    pub fn main_dir(&mut self, mut path: PathBuf) -> PathBuf {
        if self.sess.config.is_bin() {
            if let Some(bin_path) = &self.sess.config.project.bin {
                path.push(bin_path.parent().unwrap_or_else(|| "src".as_ref()));
            } else {
                path.push("src");
            }
        } else {
            if let Some(lib_path) = &self.sess.config.project.lib {
                path.push(lib_path.parent().unwrap_or_else(|| "src".as_ref()));
            } else {
                path.push("src");
            }
        }

        path
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
                _ => return None,
            };

            let dep = match self.sess.config.dependencies.get(&dep_name.clone()) {
                Some(dep) => dep,
                None => {
                    self.ctx.emit_impl(DependencyNotFound {
                        span: (span, self.file),
                        dep: dep_name.to_string(),
                    });
                    return None;
                }
            };

            if Dependency::is_special(&dep_name) {
                path_buf = if dep_name == "std" {
                    normalize_path(&self.sess.config.build.std, &self.sess.cwd)
                } else {
                    normalize_path(&self.sess.config.build.core, &self.sess.cwd)
                };
            } else {
                let base_path = if let Some(path) = &dep.path {
                    normalize_path(&PathBuf::from(path), &self.sess.cwd)
                } else if let Some(github) = &dep.github {
                    self.sess.dep_dir(&format!(
                        "{}-{}",
                        github,
                        dep.branch.as_deref().unwrap_or("main")
                    ))
                } else {
                    let dep_name = if let Some(ver) = &dep.version {
                        format!("{}-{}", dep_name, ver)
                    } else {
                        dep_name.to_string()
                    };
                    self.sess.dep_dir(&dep_name)
                };

                path_buf.push(base_path);
            }
            let main = self.main_dir(path_buf.clone());
            path_buf = self.build_filename(main, path);
        }

        Some(path_buf.with_extension("brim"))
    }

    pub fn build_filename(&mut self, path: PathBuf, paths: &Vec<PathItemKind>) -> PathBuf {
        let mut path_buf = path.clone();

        if paths.len() > 1 {
            for part in paths.iter().skip(1) {
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
            if self.sess.config.is_bin() {
                path_buf.push("main.brim");
            } else {
                path_buf.push("lib.brim");
            }
        }

        path_buf
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

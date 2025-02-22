use crate::{CompiledModules, session::Session};
use anyhow::Result;
use brim_ast::item::{ItemKind, PathItemKind};
use brim_fs::loader::BrimFileLoader;
use brim_middle::{Location, ModuleId, modules::ModuleMap, temp_diag::TemporaryDiagnosticContext};
use std::path::PathBuf;
use tracing::debug;

#[derive(Debug)]
pub struct ImportResolver<'a> {
    pub ctx: &'a mut TemporaryDiagnosticContext,
    pub map: ModuleMap,
    pub temp_loader: BrimFileLoader,
    pub file: usize,
    pub sess: &'a mut Session,
    pub compiled: CompiledModules,
}

impl<'a> ImportResolver<'a> {
    pub fn new(
        ctx: &'a mut TemporaryDiagnosticContext,
        sess: &'a mut Session,
        compiled: CompiledModules,
        map: ModuleMap,
    ) -> Self {
        Self {
            ctx,
            map,
            temp_loader: BrimFileLoader,
            file: 0,
            sess,
            compiled,
        }
    }

    pub fn resolve(&mut self) -> Result<ModuleMap> {
        for module in self.map.modules.iter_mut() {
            for item in module.barrel.items.iter_mut() {
                if let ItemKind::Use(use_stmt) = &mut item.kind {
                    let path = if use_stmt.path[0] == PathItemKind::Current {
                        let mut path = self.sess.config.cwd.clone();
                        path.push(self.sess.config.main_dir());
                        path
                    } else {
                        let dep_name = use_stmt.path[0].ident().to_string();
                        let project = self.compiled.map.get(&dep_name).unwrap();
                        let mut path = project.config.cwd.clone();
                        path.push(project.config.main_dir());

                        let file_path = ImportResolver::build_path(use_stmt.path[1..].to_vec());
                        path.push(file_path);

                        path
                    };
                    let path = path.with_extension("brim");
                    debug!("Resolving import: {:?}", path);

                    use_stmt.resolved = Some(path.clone());
                    println!("Adding import: {:?}", path);
                }
            }
        }

        Ok(self.map.clone())
    }

    pub fn build_path(parts: Vec<PathItemKind>) -> PathBuf {
        let mut path = PathBuf::new();
        for item in parts {
            match item {
                PathItemKind::Module(ident) => path.push(ident.to_string()),
                PathItemKind::Parent => {
                    path.pop();
                }
                _ => unreachable!(),
            }
        }

        path.clone()
    }
}

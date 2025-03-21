use crate::{CompiledModules, session::Session};
use anyhow::Result;
use brim_ast::item::{ItemKind, PathItemKind};
use brim_fs::{canonicalize_path, loader::BrimFileLoader};
use brim_middle::{modules::ModuleMap, temp_diag::TemporaryDiagnosticContext};
use brim_span::files::get_path;
use std::path::PathBuf;
use tracing::debug;

#[derive(Debug)]
pub struct ImportResolver<'a> {
    pub ctx: &'a mut TemporaryDiagnosticContext,
    pub map: ModuleMap,
    pub temp_loader: BrimFileLoader,
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
            sess,
            compiled,
        }
    }

    pub fn resolve(&mut self) -> Result<ModuleMap> {
        for module in self.map.modules.iter_mut() {
            for item in module.barrel.items.iter_mut() {
                if let ItemKind::Use(use_stmt) = &mut item.kind {
                    let mut current_file = get_path(module.barrel.file_id)?;
                    current_file.pop();

                    let mut path = if use_stmt.path[0] == PathItemKind::Current {
                        let path = current_file.clone();

                        path
                    } else {
                        let dep_name = use_stmt.path[0].ident().to_string();
                        let project = self.compiled.map.get(&dep_name).unwrap();
                        let mut path = project.config.cwd.clone();
                        path.push(project.config.main_dir());

                        path
                    };
                    let file_path = ImportResolver::build_path(use_stmt.path[1..].to_vec());
                    path.push(file_path);

                    let path = path.with_extension("brim");
                    debug!("Resolving import: {:?}", path);

                    use_stmt.resolved = Some(canonicalize_path(path.clone())?);
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
                PathItemKind::Parent => path.push(".."),
                _ => unreachable!(),
            }
        }

        path.clone()
    }
}

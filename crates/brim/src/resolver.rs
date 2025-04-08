use crate::{CompiledModules, session::Session};
use anyhow::Result;
use brim_ast::item::ItemKind;
use brim_fs::{canonicalize_path, loader::BrimFileLoader};
use brim_middle::{modules::ModuleMap, temp_diag::TemporaryDiagnosticContext};
use brim_span::files::get_path;
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

                    let path = if use_stmt.is_dep() {
                        let dep_name = use_stmt.path.clone();
                        let project = self.compiled.map.get(&dep_name).unwrap();
                        let mut path = project.config.cwd.clone();
                        path.push(project.config.main_dir());

                        path
                    } else {
                        current_file.join(use_stmt.path.clone())
                    };

                    debug!("Resolving import: {:?}", path);

                    use_stmt.resolved = Some(canonicalize_path(path.clone())?);
                }
            }
        }

        Ok(self.map.clone())
    }
}

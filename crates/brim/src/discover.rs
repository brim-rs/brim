use crate::session::Session;
use anyhow::Result;
use brim_ast::item::ItemKind;
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_fs::loader::{BrimFileLoader, FileLoader};
use brim_middle::{barrel::Barrel, modules::ModuleMap, temp_diag::TemporaryDiagnosticContext};
use brim_parser::parser::Parser;
use brim_span::{files::get_path, span::Span};
use std::{collections::HashSet, path::PathBuf};
use tracing::debug;

#[derive(Debug)]
pub struct ModuleDiscover<'a> {
    pub ctx: &'a mut TemporaryDiagnosticContext,
    pub map: ModuleMap,
    pub temp_loader: BrimFileLoader,
    pub file: usize,
    pub sess: &'a mut Session,
}

impl<'a> ModuleDiscover<'a> {
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
        barrel: &Barrel,
        file_id: usize,
        visited: &mut HashSet<PathBuf>,
    ) -> Result<ModuleMap> {
        self.file = barrel.file_id;
        let mut module_paths = Vec::new();

        let current_path = get_path(self.file)?;
        debug!("Resolving module declarations in file: {:?}", current_path);

        self.map
            .insert_or_update(current_path.clone(), barrel.clone());

        if visited.contains(&current_path) {
            return Ok(self.map.clone());
        }

        visited.insert(current_path.clone());

        for item in &barrel.items {
            if let ItemKind::Module(mod_decl) = &item.kind {
                let mut path = current_path.clone();
                path.pop();

                for ident in &mod_decl.idents {
                    path.push(ident.name.to_string());
                }
                path = path.with_extension("brim");

                module_paths.push((
                    mod_decl.span,
                    mod_decl.idents.last().unwrap().name.to_string(),
                    path,
                ));
            }
        }

        for (span, original_name, path) in module_paths {
            if !path.exists() {
                self.ctx.emit_impl(ModuleNotFound {
                    span: (span, barrel.file_id),
                    original_name,
                    path: path.to_string_lossy().to_string(),
                });
                continue;
            }

            let content = self.temp_loader.read_file(&path)?;
            let file = self.sess.add_file(path.clone(), content);

            let mut parser = Parser::new(file, self.sess.config.experimental.clone());
            let new_barrel = parser.parse_barrel()?;
            self.ctx.extend(parser.dcx.diags);

            self.create_module_map(&new_barrel, new_barrel.file_id, visited)?;
        }
        Ok(self.map.clone())
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

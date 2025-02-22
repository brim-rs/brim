use crate::{CompiledProjects, session::Session};
use anyhow::Result;
use brim_ast::item::{Ident, ImportsKind, ItemKind, PathItemKind, Use};
use brim_config::toml::{Config, Dependency};
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_fs::{
    canonicalize_path,
    loader::{BrimFileLoader, FileLoader},
    normalize_path,
};
use brim_hir::transformer::HirModuleMap;
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
    fs::canonicalize,
    path::{Path, PathBuf},
};

#[derive(Debug)]
pub struct ModuleDiscover<'a> {
    pub ctx: &'a mut TemporaryDiagnosticContext,
    pub map: ModuleMap,
    pub temp_loader: BrimFileLoader,
    pub file: usize,
    pub sess: &'a mut Session,
    pub compiled: CompiledProjects,
}

impl<'a> ModuleDiscover<'a> {
    pub fn new(
        ctx: &'a mut TemporaryDiagnosticContext,
        sess: &'a mut Session,
        compiled: CompiledProjects,
    ) -> Self {
        Self {
            ctx,
            map: ModuleMap::new(),
            temp_loader: BrimFileLoader,
            file: 0,
            sess,
            compiled,
        }
    }

    pub fn create_module_map(
        &mut self,
        barrel: &mut Barrel,
        visited: &mut HashSet<PathBuf>,
    ) -> Result<ModuleMap> {
        self.file = barrel.file_id;

        for item in barrel.items.clone().iter_mut() {
            if let ItemKind::Module(mod_decl) = &mut item.kind {
                let mut path = get_path(self.file)?;

                if visited.contains(&path) {
                    continue;
                }

                visited.insert(path.clone());
                self.map.insert_or_update(path.clone(), barrel.clone());

                path.pop(); // Remove the file name

                for ident in mod_decl.idents.clone() {
                    path.push(ident.name.to_string());
                }
                path = path.with_extension("brim");

                if !path.exists() {
                    self.ctx.emit_impl(ModuleNotFound {
                        span: (mod_decl.span, barrel.file_id),
                        original_name: mod_decl.idents.last().unwrap().name.to_string(),
                        path: path.to_string_lossy().to_string(),
                    });
                }

                let mut parser = Parser::new(self.file, self.sess.config.experimental.clone());
                let mut barrel = parser.parse_barrel()?;
                self.ctx.extend(parser.dcx.diags);

                self.create_module_map(&mut barrel, visited)?;
            }
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

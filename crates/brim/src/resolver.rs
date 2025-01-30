use brim_ast::item::{ItemKind, PathItemKind};
use brim_ctx::{GlobalSymbolId, ModuleId, compiler::CompilerContext};
use brim_fs::loader::{BrimFileLoader, FileLoader};
use brim_middle::{barrel::Barrel, modules::ModuleMap};
use brim_parser::parser::Parser;
use brim_span::files::{add_file, get_path};
use std::{collections::HashSet, path::PathBuf};

#[derive(Debug)]
pub struct Resolver<'a> {
    pub ctx: &'a mut CompilerContext<'a>,
    pub map: ModuleMap,
    pub temp_loader: BrimFileLoader,
}

impl<'a> Resolver<'a> {
    pub fn new(ctx: &'a mut CompilerContext<'a>) -> Self {
        Self {
            ctx,
            map: ModuleMap::new(),
            temp_loader: BrimFileLoader,
        }
    }

    pub fn create_module_map(
        &mut self,
        barrel: &mut Barrel,
        visited: &mut HashSet<PathBuf>,
    ) -> anyhow::Result<ModuleMap> {
        let file_id = barrel.file_id.clone();
        let mut ref_path = get_path(file_id.clone())?;

        if visited.contains(&ref_path) {
            return Ok(self.map.clone());
        }

        visited.insert(ref_path.clone());
        self.map.insert_or_update(ref_path.clone(), barrel.clone());

        for item in barrel.items.iter_mut() {
            if let ItemKind::Use(use_stmt) = &mut item.kind {
                let path = self.build_path(&use_stmt.path);

                ref_path.pop();
                ref_path.push(path);
                ref_path.set_extension("brim");

                let contents = self.temp_loader.read_file(&ref_path)?;
                let file = add_file(ref_path.clone(), contents);
                let mut parser = Parser::new(file);
                let mut nested_barrel = parser.parse_barrel(self.ctx)?;

                for diag in &parser.diags.dcx.diags {
                    self.ctx.emit_diag(diag.clone());
                }

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

    pub fn build_path(&self, path: &Vec<PathItemKind>) -> PathBuf {
        let mut path_buf = PathBuf::new();

        for part in path {
            match part {
                PathItemKind::Module(ident) => {
                    path_buf.push(ident.name.as_str().expect("expected module name"))
                }
                PathItemKind::Parent => {
                    path_buf.pop();
                }
            }
        }

        path_buf
    }
}

use std::path::PathBuf;
use brim_ast::item::{ItemKind, PathItemKind};
use brim_ctx::barrel::Barrel;
use brim_ctx::compiler::CompilerContext;
use brim_ctx::modules::{ModuleMap};
use brim_fs::loader::{BrimFileLoader, FileLoader};
use brim_parser::parser::Parser;
use brim_span::files::{add_file, get_path};

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
            map: ModuleMap {
                modules: Vec::new(),
                symbols: Default::default(),
            },
            temp_loader: BrimFileLoader,
        }
    }

    pub fn create_module_map(&mut self, barrel: &mut Barrel) -> anyhow::Result<()> {
        let file_id = barrel.file_id.clone();

        let items = std::mem::take(&mut barrel.items);

        for mut item in items {
            match &mut item.kind {
                ItemKind::Use(use_stmt) => {
                    let path = self.build_path(&use_stmt.path);

                    // we create the path based on the provided barrel
                    let mut ref_path = get_path(file_id.clone())?;
                    self.map.insert_or_update(ref_path.clone(), barrel.clone());

                    // remove the file name
                    ref_path.pop();

                    ref_path.push(path);
                    ref_path.set_extension("brim");

                    let contents = self.temp_loader.read_file(&ref_path)?;
                    let file = add_file(ref_path.clone(), contents);

                    let parser = &mut Parser::new(file);
                    let mut nested_barrel = parser.parse_barrel(self.ctx)?;

                    for diag in &parser.diags.dcx.diags {
                        self.ctx.emit_diag(diag.clone());
                    }

                    self.map.insert_or_update(ref_path.clone(), nested_barrel.clone());
                    use_stmt.resolved_path = Some(ref_path);

                    self.create_module_map(&mut nested_barrel)?;
                }
                _ => {}
            }
            barrel.items.push(item);
        }

        Ok(())
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

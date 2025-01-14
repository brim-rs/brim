use crate::{barrel::Barrel, compiler::CompilerContext, walker::AstWalker};
use brim_ast::item::{PathItemKind, Use};
use std::path::PathBuf;

#[derive(Debug)]
pub struct Resolver<'a> {
    pub ctx: &'a mut CompilerContext<'a>,
}

impl<'a> Resolver<'a> {
    pub fn create_module_map(&mut self, barrel: &mut Barrel) {
        for item in &mut barrel.items {
            self.walk_item(item);
        }
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

impl<'a> AstWalker for Resolver<'a> {
    fn visit_use(&mut self, use_stmt: &mut Use) {
        let path = self.build_path(&use_stmt.path);

        println!("Resolved path: {:?}", path);
    }
}

use brim_ast::item::{Block, Generics, Item, Use};
use crate::barrel::Barrel;
use crate::compiler::CompilerContext;
use crate::walker::AstWalker;

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
}

impl<'a> AstWalker for Resolver<'a> {
    fn visit_use(&mut self, use_stmt: &mut Use) {
        self.ctx.imports.push(use_stmt.clone());
    }
}
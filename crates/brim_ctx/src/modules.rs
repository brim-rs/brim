use crate::{barrel::Barrel, compiler::CompilerContext, walker::AstWalker};
use std::path::PathBuf;

#[derive(Debug)]
pub struct Module {
    pub path: PathBuf,
    pub barrel: Barrel,
}

#[derive(Debug)]
pub struct ModuleMap {
    pub modules: Vec<Module>,
}

impl ModuleMap {
    pub fn insert_or_update(&mut self, path: PathBuf, barrel: Barrel) {
        for module in &mut self.modules {
            if module.path == path {
                module.barrel = barrel;
                return;
            }
        }

        self.modules.push(Module { path, barrel });
    }
}


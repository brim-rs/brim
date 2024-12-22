pub mod compiler;
mod generator;
mod utils;
mod functions;

use std::fs::{create_dir_all, remove_file, File};
use std::io::Write;
use crate::compilation::imports::UnitLoader;
use crate::compilation::unit::CompilationUnit;
use anyhow::Result;
use tracing::debug;
use crate::compilation::build_type::BuildType;
use crate::context::GlobalContext;
use crate::path::strip_base;

pub struct CodeGen<'a> {
    pub unit: &'a mut CompilationUnit,
    pub loader: &'a mut UnitLoader,
    pub build_type: BuildType,
    // If the file is either main.brim or lib.brim
    pub is_entry_point: bool,
    pub buf: Vec<u8>,
    pub ident: usize,
    pub current_indent: usize,
    pub needed_imports: Vec<String>,
}

impl<'a> CodeGen<'a> {
    pub fn new(unit: &'a mut CompilationUnit, loader: &'a mut UnitLoader, build_type: BuildType, entrypoint: bool) -> Result<Self> {
        Ok(Self {
            unit,
            loader,
            build_type,
            is_entry_point: entrypoint,
            buf: Vec::new(),
            ident: 0,
            current_indent: 0,
            needed_imports: vec![],
        })
    }
}

impl<'a> CodeGen<'a> {
    pub fn generate_code(&mut self, global: &mut GlobalContext) -> Result<()> {
        for item in &self.unit.ast().top_level_items.cloned_indices() {
            let item = self.unit.ast().query_item(*item).clone();

            self.generate_item(item)?;
        }

        debug!("Generated C++ code: \n{}", String::from_utf8_lossy(&self.buf));

        Ok(())
    }

    pub fn generate_and_write(&mut self, global: &mut GlobalContext) -> Result<()> {
        self.generate_code(global)?;
        let out_dir = global.build_dir()?.join("source");
        let rest_path = strip_base(&self.unit.source.path, &global.cwd);

        let target_path = out_dir.join(rest_path).with_extension("cpp");

        if let Some(parent_dir) = target_path.parent() {
            create_dir_all(parent_dir)?;
        }

        debug!("Writing generated code to {}", target_path.display());

        let mut file = File::create(target_path)?;
        file.write_all(&self.buf)?;

        Ok(())
    }
}
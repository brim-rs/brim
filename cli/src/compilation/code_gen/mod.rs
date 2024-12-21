pub mod compiler;
mod generator;

use std::fs::{create_dir_all, remove_file, File};
use std::io::Write;
use crate::compilation::imports::UnitLoader;
use crate::compilation::unit::CompilationUnit;
use anyhow::Result;
use crate::compilation::build_type::BuildType;
use crate::context::GlobalContext;

pub struct CodeGen<'a> {
    pub unit: &'a mut CompilationUnit,
    pub loader: &'a mut UnitLoader,
    pub build_type: BuildType,
    // If the file is either main.brim or lib.brim
    pub is_entry_point: bool,
}

impl<'a> CodeGen<'a> {
    pub fn new(unit: &'a mut CompilationUnit, loader: &'a mut UnitLoader, build_type: BuildType, entrypoint: bool) -> Result<Self> {
        Ok(Self {
            unit,
            loader,
            build_type,
            is_entry_point: entrypoint,
        })
    }
}

impl<'a> CodeGen<'a> {
    pub fn generate_code(&mut self, global: &mut GlobalContext) -> Result<()> {
        for item in &self.unit.ast().top_level_items.cloned_indices() {
            let item = self.unit.ast().query_item(*item).clone();

            self.generate_item(item)?;
        }

        Ok(())
    }

    pub fn write_and_link(&mut self, global: &mut GlobalContext, bytes: Vec<u8>) -> Result<()> {
        let file_name = global.config.project.name.clone();
        let output_file = global.output_file(&file_name)?;
        remove_file(&output_file).ok();

        create_dir_all(output_file.parent().unwrap())?;
        let mut file = File::create(&output_file)?;
        file.write_all(&bytes)?;

        let compiler = compiler::detect_compiler()?;

        global.shell.status("Detected", compiler.to_string())?;
        let exec_name = format!("{}{}", file_name, if cfg!(windows) { ".exe" } else { "" });

        Ok(())
    }
}
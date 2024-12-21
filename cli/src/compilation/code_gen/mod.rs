pub mod compiler;
mod generator;
mod utils;

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
    pub buf: Vec<u8>,
}

impl<'a> CodeGen<'a> {
    pub fn new(unit: &'a mut CompilationUnit, loader: &'a mut UnitLoader, build_type: BuildType, entrypoint: bool) -> Result<Self> {
        Ok(Self {
            unit,
            loader,
            build_type,
            is_entry_point: entrypoint,
            buf: Vec::new(),
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
}
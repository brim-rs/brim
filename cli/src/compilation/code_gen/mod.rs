mod enums;
mod expr;
mod functions;
mod generator;
mod structs;
mod utils;

use crate::{
    compilation::{imports::UnitLoader, unit::CompilationUnit},
    context::GlobalContext,
    path::strip_base,
};
use anyhow::{bail, Result};
use brim_config::OptLevel;
use brim_cpp_compiler::CppBuild;
use std::{
    fs::{create_dir_all, File},
    io::Write,
};
use tracing::debug;

pub struct CodeGen<'a> {
    pub unit: &'a mut CompilationUnit,
    pub loader: &'a mut UnitLoader,
    pub build_type: OptLevel,
    // If the file is either main.brim or lib.brim
    pub is_entry_point: bool,
    pub buf: Vec<u8>,
    pub ident: usize,
    pub current_indent: usize,
    pub needed_imports: Vec<String>,
    pub is_bin: bool,
    pub fn_return_type: Option<String>,
    pub main: bool,
}

impl<'a> CodeGen<'a> {
    pub fn new(
        unit: &'a mut CompilationUnit,
        loader: &'a mut UnitLoader,
        build_type: OptLevel,
        entrypoint: bool,
    ) -> Result<Self> {
        Ok(Self {
            unit,
            loader,
            build_type,
            is_entry_point: entrypoint,
            buf: Vec::new(),
            ident: 0,
            current_indent: 0,
            needed_imports: vec![],
            is_bin: false,
            fn_return_type: None,
            main: false,
        })
    }
}

impl<'a> CodeGen<'a> {
    pub fn generate_code(&mut self, global: &mut GlobalContext) -> Result<()> {
        self.is_bin = global.is_bin()?;

        let namespace = &self.unit.namespace;
        // We wrap every file in a namespace to avoid name collisions... going to have to think about this one
        self.write_line(format!("namespace {} {{", namespace));
        self.push_indent();

        for item in &self.unit.ast().top_level_items.cloned_indices() {
            let item = self.unit.ast().query_item(*item).clone();

            self.generate_item(item)?;
        }

        self.pop_indent();
        self.write_line("}");

        self.inject_imports();

        self.main = true;

        if self.is_entry_point && self.is_bin {
            let main_fn = self.unit.ast().main_fn();

            if let Some(main_fn_id) = main_fn {
                let main_fn = self
                    .unit
                    .ast()
                    .query_stmt(main_fn_id)
                    .clone()
                    .as_function()
                    .clone();
                self.generate_fn(main_fn)?;
            } else {
                bail!("Entrypoint file must contain a main function.");
            }
        }

        debug!(
            "Generated C++ code: \n{}",
            String::from_utf8_lossy(&self.buf)
        );

        Ok(())
    }

    pub fn generate_and_write(
        &mut self,
        global: &mut GlobalContext,
        build_cpp: &mut CppBuild,
    ) -> Result<()> {
        self.generate_code(global)?;
        let out_dir = global.build_dir()?.join("source");
        let rest_path = strip_base(&self.unit.source.path, &global.cwd);

        let target_path = out_dir.join(rest_path).with_extension("cpp");

        if let Some(parent_dir) = target_path.parent() {
            create_dir_all(parent_dir)?;
        }

        debug!("Writing generated code to {}", target_path.display());

        let mut file = File::create(target_path.clone())?;
        file.write_all(&self.buf)?;

        build_cpp.add_source(target_path);

        Ok(())
    }
}

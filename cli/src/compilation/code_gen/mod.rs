pub mod linker;

use std::fmt::format;
use std::fs::{create_dir_all, remove_file, File};
use std::io::Write;
use crate::compilation::imports::UnitLoader;
use crate::compilation::unit::CompilationUnit;
use crate::error::diagnostic::Diagnostics;
use anyhow::Result;
use cranelift::codegen::settings::OptLevel;
use crate::compilation::build_type::BuildType;
use cranelift::prelude::*;
use cranelift::prelude::settings::Builder;
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use crate::compilation::code_gen::linker::resolve_from_kind;
use crate::context::GlobalContext;

#[derive(Debug)]
pub struct CodeGen<'a> {
    pub unit: &'a mut CompilationUnit,
    pub loader: &'a mut UnitLoader,
    pub build_type: BuildType,
}

impl<'a> CodeGen<'a> {
    pub fn new(unit: &'a mut CompilationUnit, loader: &'a mut UnitLoader, build_type: BuildType) -> Self {
        Self {
            unit,
            loader,
            build_type,
        }
    }
}

impl<'a> CodeGen<'a> {
    pub fn settings(&self) -> Result<Builder> {
        let mut builder = settings::builder();

        match self.build_type {
            BuildType::Debug => {
                builder.set("opt_level", "none")?;
            }
            BuildType::Release => {
                builder.set("opt_level", "speed_and_size")?;
            }
        }

        Ok(builder)
    }

    pub fn generate_code(&mut self, global: &mut GlobalContext) -> Result<()> {
        let settings = self.settings()?;
        let flags = settings::Flags::new(settings);
        let isa_builder = cranelift_native::builder().unwrap();
        let isa = isa_builder.finish(flags).unwrap();

        let mut signature = Signature::new(isa::CallConv::SystemV);

        signature.returns.push(AbiParam::new(types::I64));

        let object_builder =
            ObjectBuilder::new(isa, "main", cranelift_module::default_libcall_names()).unwrap();
        let mut module = ObjectModule::new(object_builder);

        let function_id = module
            .declare_function("main", Linkage::Export, &signature)
            .unwrap();

        let mut function = codegen::ir::Function::with_name_signature(
            codegen::ir::UserFuncName::user(0, 0),
            signature,
        );
        let mut func_ctx = FunctionBuilderContext::new();
        let mut function_builder = FunctionBuilder::new(&mut function, &mut func_ctx);

        let block = function_builder.create_block();
        function_builder.seal_block(block);
        function_builder.append_block_params_for_function_params(block);
        function_builder.switch_to_block(block);

        let return_value = function_builder.ins().iconst(types::I64, 42);
        function_builder.ins().return_(&[return_value]);

        function_builder.finalize();

        let mut ctx = codegen::Context::for_function(function);

        module.define_function(function_id, &mut ctx).unwrap();

        let object_product = module.finish();
        let bytes = object_product.emit()?;

        let file_name = global.config.project.name.clone();
        
        let output_file = global.output_file(&file_name)?;
        remove_file(&output_file).ok();

        create_dir_all(output_file.parent().unwrap())?;
        let mut file = File::create(&output_file)?;
        file.write_all(&bytes)?;

        let linker = if let Some(build) = &global.config.build && let Some(linker) = &build.linker {
            resolve_from_kind(linker.clone())?
        } else {
            linker::detect_linker()?
        };
        
        global.shell.status("Detected", linker.to_string())?;
        let exec_name = format!("{}{}", file_name, if cfg!(windows) { ".exe" } else { "" });

        linker.link(exec_name, vec![output_file], global.build_dir()?)?;

        Ok(())
    }
}
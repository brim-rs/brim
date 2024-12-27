use std::vec;

use crate::{
    ast::{
        expressions::CallExpr,
        statements::{Function, Generic, TypeAnnotation},
        types::TypeKind,
    },
    compilation::code_gen::CodeGen,
};
use anyhow::Result;
use tracing::debug;
use brim_cpp_compiler::CppBuild;
use crate::context::GlobalContext;

impl<'a> CodeGen<'a> {
    pub fn generate_fn(&mut self, function: Function, global: &mut GlobalContext, build_cpp: &mut CppBuild) -> Result<()> {
        let mut return_type = self.map_type(function.clone().return_type, vec![]);

        if function.name.literal() == "main" && self.is_entry_point {
            if self.is_bin {
                return_type = "int".to_string();
            }

            if !self.main {
                return Ok(());
            }
        }

        let mut params = vec![];

        for param in function.params {
            let param_type = self.map_type(Some(param.type_annotation), vec![]);
            params.push(format!("{} {}", param_type, param.ident.literal()));
        }

        let generics = function.generics;
        if generics.len() > 0 {
            let mut text = vec![];

            for generic in &generics {
                let name = generic.name.literal();
                if let Some(typ) = generic.type_annotation.clone() {
                    let typ = self.map_type(Some(typ), vec![]);

                    text.push(format!("typename {} = {}", name, typ));
                } else {
                    text.push(format!("typename {}", name));
                }
            }

            self.write_line(format!("template <{}>", text.join(", ")));
        }

        self.write_line(format!(
            "{} {} ({}) {{",
            return_type,
            function.name.literal(),
            params.join(", ")
        ));

        self.push_indent();

        if let Some(body) = function.body {
            let body = self.unit.ast().query_stmt(body).clone();

            self.fn_return_type = Some(return_type);
            self.generate_stmt(body, global, build_cpp)?;
            self.fn_return_type = None;
        }

        self.pop_indent();
        self.write_line("}");

        Ok(())
    }

    pub fn map_type(&mut self, typ: Option<TypeAnnotation>, generics: Vec<Generic>) -> String {
        if let Some(typ) = typ {
            let kind_str = self.map_kind(&typ.kind, generics.clone());

            if typ.can_be_error {
                let error_type = if let Some(error_type) = typ.error_type {
                    self.map_type(Some(*error_type), generics)
                } else {
                    "Result<(), ()>".to_string()
                };

                format!("Result<{}, {}>", kind_str, error_type)
            } else {
                kind_str
            }
        } else {
            "void".to_string()
        }
    }

    fn map_kind(&mut self, kind: &TypeKind, generics: Vec<Generic>) -> String {
        if kind.is_number() {
            self.needed_imports.push("cstdint".to_string());
        }

        match kind {
            TypeKind::Char => "char".to_string(),
            TypeKind::String => {
                self.needed_imports.push("string".to_string());
                "std::string".to_string()
            }
            TypeKind::U8 => "uint8_t".to_string(),
            TypeKind::U16 => "uint16_t".to_string(),
            TypeKind::U32 => "uint32_t".to_string(),
            TypeKind::U64 => "uint64_t".to_string(),

            TypeKind::I8 => "int8_t".to_string(),
            TypeKind::I16 => "int16_t".to_string(),
            TypeKind::I32 => "int32_t".to_string(),
            TypeKind::I64 => "int64_t".to_string(),

            TypeKind::Isize => "intptr_t".to_string(),
            TypeKind::Usize => "uintptr_t".to_string(),

            TypeKind::Custom(ident) => {
                let generic = generics.iter().find(|g| g.name.literal() == ident.clone());
                if let Some(generic) = generic
                    && let Some(typ) = generic.type_annotation.clone()
                {
                    self.map_type(Some(typ), generics)
                } else {
                    ident.to_string()
                }
            }

            TypeKind::Bool => "bool".to_string(),

            _ => {
                debug!("Unknown type: {:?}. Defaulting to auto", kind);

                "auto".to_string()
            }
        }
    }

    pub fn generate_call(&mut self, call: CallExpr) -> Result<()> {
        if call.is_builtin {
            self.generate_built_in(call)?;
            
            return Ok(());
        }

        if let Some(x) = self.unit.unit_items.get(&call.callee) {
            let unit_data = x.unit.clone();
            let (_, unit) = self.loader.load_unit(&unit_data, self.unit)?;

            let namespace = &unit.namespace;
            self.write(format!("{}::{}(", namespace, call.callee));
        } else {
            self.write(format!("{}(", call.callee));
        }

        for (i, arg) in call.args.iter().enumerate() {
            let arg = self.unit.ast().query_expr(*arg).clone();
            self.generate_expr(arg)?;

            if i < call.args.len() - 1 {
                self.write(", ");
            }
        }

        self.write(")");

        Ok(())
    }
}

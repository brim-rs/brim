use crate::ast::item::TopLevelItem;
use crate::compilation::code_gen::CodeGen;
use anyhow::Result;
use tracing::debug;
use crate::ast::statements::{Function, Stmt, StmtKind, TypeAnnotation};
use crate::ast::types::TypeKind;
use crate::context::GlobalContext;

impl<'a> CodeGen<'a> {
    pub fn generate_fn(&mut self, function: Function) -> Result<()> {
        // TODO: implement generics for functions
        let mut return_type = self.map_type(function.return_type, vec![]);

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
            self.generate_stmt(body)?;
            self.fn_return_type = None;
        }

        self.pop_indent();
        self.write_line("}");

        Ok(())
    }

    pub fn map_type(&mut self, typ: Option<TypeAnnotation>, generics: Vec<String>) -> String {
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

    fn map_kind(&mut self, kind: &TypeKind, generics: Vec<String>) -> String {
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
                if generics.contains(&ident) {
                    ident.to_string()
                } else {
                    "void".to_string()
                }
            }
            
            TypeKind::Bool => "bool".to_string(),
            

            _ => {
                debug!("Unknown type: {:?}. Defaulting to auto", kind);

                "auto".to_string()
            },
        }
    }
}

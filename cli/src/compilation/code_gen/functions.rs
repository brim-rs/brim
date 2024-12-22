use crate::ast::item::TopLevelItem;
use crate::compilation::code_gen::CodeGen;
use anyhow::Result;
use crate::ast::statements::{Function, Stmt, StmtKind, TypeAnnotation};
use crate::ast::types::TypeKind;

impl<'a> CodeGen<'a> {
    pub fn generate_fn(&mut self, function: Function) -> Result<()> {
        let return_type = self.map_type(function.return_type);
        let mut params = vec![];

        for param in function.params {
            let param_type = self.map_type(Some(param.type_annotation));
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
            
            self.generate_stmt(body)?;
        }

        self.pop_indent();
        self.write_line("}");

        Ok(())
    }

    pub fn map_type(&mut self, typ: Option<TypeAnnotation>) -> String {
        if let Some(typ) = typ {
            let kind_str = self.map_kind(&typ.kind);

            if typ.can_be_error {
                let error_type = if let Some(error_type) = typ.error_type {
                    self.map_type(Some(*error_type))
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

    fn map_kind(&mut self, kind: &TypeKind) -> String {
        match kind {
            TypeKind::Char => "char".to_string(),
            TypeKind::String => {
                self.needed_imports.push("std::string".to_string());
                "std::string".to_string()
            }
            TypeKind::U8 => {
                self.needed_imports.push("<cstdint>".to_string());
                "uint8_t".to_string()
            }
            TypeKind::U16 => "uint16_t".to_string(),
            TypeKind::U32 => "uint32_t".to_string(),
            TypeKind::U64 => "uint64_t".to_string(),
            TypeKind::I8 => {
                self.needed_imports.push("<cstdint>".to_string());
                "int8_t".to_string()
            }
            TypeKind::I16 => "int16_t".to_string(),
            TypeKind::I32 => "int32_t".to_string(),
            TypeKind::I64 => "int64_t".to_string(),

            TypeKind::Isize => {
                self.needed_imports.push("<cstdint>".to_string());
                "intptr_t".to_string()
            }
            TypeKind::Usize => "uintptr_t".to_string(),
            _ => "void".to_string(),
        }
    }
}

use crate::{
    ast::{expressions::StructConstructor, statements::Struct},
    compilation::{code_gen::CodeGen, items::UnitItemKind},
};
use anyhow::Result;
use brim_cpp_compiler::{compiler::CompilerKind, CppBuild};
use indexmap::IndexMap;

impl<'a> CodeGen<'a> {
    pub fn generate_struct_def(&mut self, struct_def: Struct) -> Result<()> {
        let gens = struct_def.generics;
        self.generate_generic(gens.clone());

        self.write_line(format!("struct {} {{", struct_def.name.literal()));
        self.push_indent();

        for (name, field) in struct_def.fields.iter() {
            let typ = self.map_type(Some(field.type_annotation.clone()), gens.clone());
            self.write_line(format!("{} {};", typ, name));
        }

        self.pop_indent();
        self.write_line("};");

        Ok(())
    }

    pub fn generate_struct_constructor(
        &mut self,
        constructor: StructConstructor,
        build_cpp: &mut CppBuild,
    ) -> Result<()> {
        // First, collect all the data we need before any mutable borrows
        let struct_ = self.unit.unit_items.get(&constructor.name).unwrap();
        let unit_data = struct_.unit.clone();

        let struct_fields = if let UnitItemKind::Struct(struct_def) = &struct_.kind {
            struct_def.fields.clone()
        } else {
            return Err(anyhow::anyhow!("Expected struct definition"));
        };

        let (_, unit) = self.loader.load_unit(&unit_data, self.unit)?;
        let namespace = unit.namespace.clone();

        self.write(format!("{}::{}{{", namespace, constructor.name));

        // MSVC doesn't support designated initializers
        if build_cpp.compiler_kind() != &CompilerKind::Msvc {
            for (i, (name, expr_id)) in constructor.fields.iter().enumerate() {
                let field_value = self.unit.ast().query_expr(*expr_id).clone();

                self.write(name);
                self.write(": ");
                self.generate_expr(field_value, build_cpp)?;

                if i < constructor.fields.len() - 1 {
                    self.write(", ");
                }
            }
        } else {
            let field_map: IndexMap<_, _> = constructor.fields.iter().collect();

            for (i, (name, _)) in struct_fields.iter().enumerate() {
                let expr_id = field_map.get(name).unwrap();
                let field_value = self.unit.ast().query_expr(**expr_id).clone();

                self.generate_expr(field_value, build_cpp)?;

                if i < struct_fields.len() - 1 {
                    self.write(", ");
                }
            }
        }

        self.write("}");

        Ok(())
    }
}

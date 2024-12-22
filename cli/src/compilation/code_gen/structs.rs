use crate::ast::statements::Struct;
use crate::compilation::code_gen::CodeGen;
use anyhow::Result;

impl<'a> CodeGen<'a> {
    pub fn generate_struct_def(&mut self, struct_def: Struct) -> Result<()> {
        let generics = struct_def.generics.iter().map(|g| g.literal()).collect::<Vec<_>>();

        if generics.len() > 0 {
            let text: Vec<_> = generics.iter().map(|t| format!("typename {}", t)).collect();

            self.write_line(format!("template <{}>", text.join(", ")))
        }

        self.write_line(format!("struct {} {{", struct_def.name.literal()));
        self.push_indent();

        for (name, field) in struct_def.fields.iter() {
            let typ = self.map_type(Some(field.type_annotation.clone()), generics.clone());
            self.write_line(format!("{} {};", typ, name));
        }

        self.pop_indent();
        self.write_line("};");

        Ok(())
    }
}
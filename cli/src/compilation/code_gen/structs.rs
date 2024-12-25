use crate::{ast::statements::Struct, compilation::code_gen::CodeGen};
use anyhow::Result;

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
}

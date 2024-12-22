use crate::ast::statements::Enum;
use crate::compilation::code_gen::CodeGen;
use anyhow::Result;

impl<'a> CodeGen<'a> {
    pub fn generate_enum(&mut self, enum_def: Enum) -> Result<()> {
        let generics = enum_def.generics.iter().map(|g| g.literal()).collect::<Vec<_>>();

        // Generate template declaration if needed
        if !enum_def.generics.is_empty() {
            let template_params = generics.iter()
                .map(|t| format!("typename {}", t))
                .collect::<Vec<_>>()
                .join(", ");
            self.write_line(format!("template <{}>", template_params));
        }

        self.needed_imports.push("variant".to_string());
        self.write_line(format!("class {} {{", enum_def.name.literal()));
        self.write_line("public:");
        self.push_indent();

        // Generate variant structs with Type suffix
        for variant in &enum_def.variants {
            self.write_line(format!("struct {}Type {{", variant.ident.literal()));
            self.push_indent();
            for (i, typ) in variant.params.iter().enumerate() {
                let mapped_type = self.map_type(Some(typ.clone()), generics.clone());
                self.write_line(format!("{} m_{};", mapped_type, i));
            }
            self.pop_indent();
            self.write_line("};");
        }

        // Generate variant type using the Type suffix
        let variants = enum_def.variants.iter()
            .map(|v| format!("{}::{}Type", enum_def.name.literal(), v.ident.literal()))
            .collect::<Vec<_>>()
            .join(", ");
        self.write_line(format!("using Variant = std::variant<{}>;", variants));

        self.pop_indent();
        self.write_line("private:");
        self.push_indent();
        self.write_line("Variant m_variant;");
        self.pop_indent();

        // Generate constructors
        self.write_line("public:");
        self.push_indent();

        let type_generics = if !generics.is_empty() {
            format!("<{}>", generics.join(", "))
        } else {
            String::new()
        };

        let constructor_base = format!("static {}{}", enum_def.name.literal(), type_generics);

        for variant in &enum_def.variants {
            let variant_name = variant.ident.literal();
            let param_names: Vec<_> = (0..variant.params.len())
                .map(|i| format!("m_{}", i))
                .collect();

            // Generate constructor parameters
            let params = variant.params.iter().enumerate()
                .map(|(i, typ)| {
                    let mapped_type = self.map_type(Some(typ.clone()), generics.clone());
                    format!("{} {}", mapped_type, param_names[i])
                })
                .collect::<Vec<_>>()
                .join(", ");

            self.write_line(format!("{} {}({}) {{", constructor_base, variant_name, params));
            self.push_indent();
            self.write_line(format!("{}{} result;", enum_def.name.literal(), type_generics));
            self.write_line(format!("result.m_variant = {}Type{{{}}};", variant_name, param_names.join(", ")));
            self.write_line("return result;");
            self.pop_indent();
            self.write_line("}");
        }

        self.pop_indent();
        self.write_line("};");

        // Generate deduction guides if there are generics
        if !generics.is_empty() {
            self.write_line("");
            for variant in &enum_def.variants {
                let variant_name = variant.ident.literal();
                let guide = format!(
                    "template<typename {0}> {1}(typename {1}<{2}>::{3}Type) -> {1}<{2}>;",
                    generics.join(", typename "), // Add "typename" keyword for each generic
                    enum_def.name.literal(),
                    generics.join(", "),
                    variant_name
                );
                self.write_line(guide);
            }
        }


        Ok(())
    }
}
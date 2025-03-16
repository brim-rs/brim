use crate::codegen::CppCodegen;
use brim_hir::{
    CompiledModules,
    items::{HirItem, HirItemKind},
};

impl CppCodegen {
    pub fn generate_item(&mut self, item: HirItem, compiled: &CompiledModules) {
        match item.kind {
            HirItemKind::Fn(decl) => {
                let ret = self.generate_ty(decl.sig.return_type);
                self.generate_generics(&decl.sig.generics);

                let params = decl
                    .sig
                    .params
                    .params
                    .iter()
                    .map(|p| {
                        format!(
                            "{} {}",
                            self.generate_ty(p.ty.kind.clone()),
                            p.name.to_string()
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                let block_name = format!("{} {}({})", ret, decl.sig.name.to_string(), params);

                let body_expr = self.hir().get_expr(decl.body.unwrap()).clone();
                let body_code = self.generate_expr(body_expr.clone());

                self.code.add_block(&block_name, |code| {
                    code.add_line_no_indent(&body_code);
                });
            }
            HirItemKind::Struct(s) => {
                self.generate_generics(&s.generics);
                self.code
                    .add_line(&format!("struct {} {{", s.ident.name.to_string()));
                self.code.increase_indent();

                for field in s.fields {
                    let ty = self.generate_ty(field.ty.clone());

                    self.code
                        .add_line(&format!("{} {};", ty, field.ident.to_string()));
                }

                self.code.decrease_indent();
                self.code.add_line("};");
            }
            HirItemKind::External(external) => {
                if let Some(abi) = external.abi {
                    self.code.add_line(&format!("extern \"{}\" {{", abi));
                } else {
                    self.code.add_line("extern {");
                }

                self.code.increase_indent();
                for item in external.items {
                    self.generate_item(compiled.get_item(item).clone(), compiled);
                }
                self.code.decrease_indent();
                self.code.add_line("}");
            }
            _ => {}
        }
    }
}

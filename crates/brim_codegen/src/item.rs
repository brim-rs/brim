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

                let name = if self.add_prefix {
                    format!("brim_{}", decl.sig.name.to_string())
                } else {
                    decl.sig.name.to_string()
                };
                let block_name = format!("{} {}({})", ret, name, params);

                if let Some(body) = decl.body {
                    let body_expr = self.hir().get_expr(body).clone();
                    let body_code = self.generate_expr(body_expr.clone());

                    self.code.add_block(&block_name, |code| {
                        code.add_line_no_indent(&body_code);
                    });
                } else {
                    self.code.add_line(&format!("{};", block_name));
                };
            }
            HirItemKind::Struct(s) => {
                self.generate_generics(&s.generics);
                let name = if self.add_prefix {
                    format!("brim_{}", s.ident.name.to_string())
                } else {
                    s.ident.name.to_string()
                };
                self.code.add_line(&format!("struct {} {{", name));
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

                let mut externs = vec![];
                self.code.increase_indent();
                self.add_prefix = false;
                for item in external.items.clone() {
                    let item = compiled.get_item(item).clone();
                    self.generate_item(item.clone(), compiled);

                    externs.push(item.ident.to_string());
                }
                self.add_prefix = true;
                self.code.decrease_indent();
                self.code.add_line("}");

                for item in external.items {
                    let item = compiled.get_item(item).clone();
                    self.generate_wrapper_item(item, compiled);
                }
            }
            _ => {}
        }
    }

    pub fn generate_wrapper_item(&mut self, item: HirItem, compiled: &CompiledModules) {
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

                let original_name = decl.sig.name.to_string();
                let wrapper_name = format!("brim_{}", original_name);
                let block_name = format!("{} {}({})", ret, wrapper_name, params);

                self.code.add_block(&block_name, |code| {
                    let param_names = decl
                        .sig
                        .params
                        .params
                        .iter()
                        .map(|p| p.name.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");

                    code.add_line(&format!("return {}({});", original_name, param_names));
                });
            }
            _ => {}
        }
    }
}

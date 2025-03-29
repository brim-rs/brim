use crate::codegen::CppCodegen;
use brim_hir::{
    CompiledModules,
    items::{HirItem, HirItemKind},
};

impl CppCodegen {
    pub fn generate_item(&mut self, item: HirItem, compiled: &CompiledModules) {
        match item.kind {
            HirItemKind::Fn(decl) => {
                let ret = self.generate_ty(decl.sig.return_type.clone());

                let generics = if let Some(str) = &self.parent_struct {
                    str.generics.join(&decl.sig.generics)
                } else {
                    decl.sig.generics.clone()
                };
                self.generate_generics(&generics);

                let params = decl.sig.params.params.clone();
                let params = params
                    .iter()
                    .map(|p| {
                        format!(
                            "{} brim_{}",
                            self.generate_ty(p.ty.kind.clone()),
                            p.name.to_string()
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                let name = if self.add_prefix {
                    if let Some(str) = &self.parent_struct {
                        format!("brim_{}_{}", str.ident, decl.sig.name.to_string())
                    } else {
                        format!("brim_{}", decl.sig.name.to_string())
                    }
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
            HirItemKind::Struct(mut s) => {
                self.generate_generics(&s.generics);
                let name = if self.add_prefix {
                    format!("brim_{}", s.ident.name.to_string())
                } else {
                    s.ident.name.to_string()
                };
                self.code.add_line(&format!("struct {} {{", name));
                self.code.increase_indent();

                for field in s.fields.clone() {
                    let ty = self.generate_ty(field.ty.clone());

                    self.code
                        .add_line(&format!("{} {};", ty, field.ident.to_string()));
                }

                for (ident, id) in s.items.clone() {
                    let item = self.compiled.get_item(id.clone());

                    if let Some(func) = item.as_fn_safe() {
                        if !func.is_static() {
                            self.generate_item(item.clone(), compiled);

                            s.items.remove(&ident);
                        }
                    }
                }

                self.code.decrease_indent();
                self.code.add_line("};");

                self.parent_struct = Some(s.clone());
                for (_, id) in s.items {
                    let item = self.compiled.get_item(id);

                    self.generate_item(item.clone(), compiled);
                }
                self.parent_struct.take();
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
            HirItemKind::Enum(e) => {
                self.generate_generics(&e.generics);
                let name = if self.add_prefix {
                    format!("brim_{}", e.ident.name.to_string())
                } else {
                    e.ident.name.to_string()
                };
                let mut variant_names = vec![];

                self.code.add_line(&format!("struct {};", name));

                for variant in &e.variants {
                    let variant_name = format!("{}_{}", name, variant.ident.name);
                    variant_names.push(variant_name.clone());

                    self.code.add_line(&format!("struct {} {{", variant_name));
                    self.code.increase_indent();

                    for (i, field) in variant.fields.iter().enumerate() {
                        let ty = self.generate_ty(field.ty.clone());
                        self.code.add_line(&format!("{} field_{};", ty, i));
                    }

                    let params: Vec<String> = variant
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| format!("{} f{}", self.generate_ty(field.ty.clone()), i))
                        .collect();

                    let init_list: Vec<String> = (0..variant.fields.len())
                        .map(|i| format!("field_{}(f{})", i, i))
                        .collect();

                    if variant.fields.is_empty() {
                        self.code
                            .add_line(&format!("{}() = default;", variant_name));
                    } else {
                        self.code.add_line(&format!(
                            "{}({}) : {} {{}}",
                            variant_name,
                            params.join(", "),
                            init_list.join(", ")
                        ));
                    }

                    self.code.decrease_indent();
                    self.code.add_line("};");
                }

                self.code.add_line(&format!("struct {} {{", name));
                self.code.increase_indent();

                self.code.add_line("std::variant<");
                for (index, variant_name) in variant_names.iter().enumerate() {
                    self.code.add_line(&format!(
                        "{}{}",
                        variant_name,
                        if index == variant_names.len() - 1 {
                            ""
                        } else {
                            ","
                        }
                    ));
                }
                self.code.add_line("> value;");

                self.code.add_line(&format!("{}() = default;", name));

                self.code.add_line(&format!(
                    "{0}(const {0}& other) : value(std::visit([](const auto& v) {{ return decltype(value)(v); }}, other.value)) {{}}",
                    name
                ));

                self.code.add_line(&format!(
                    "{0}({0}&& other) noexcept : value(std::move(other.value)) {{}}",
                    name
                ));

                for variant_name in &variant_names {
                    self.code.add_line(&format!(
                        "{}({} v) : value(std::move(v)) {{}}",
                        name, variant_name
                    ));
                }

                self.code.decrease_indent();
                self.code.add_line("};");
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
                let block_name = format!("inline {} {}({})", ret, wrapper_name, params);

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

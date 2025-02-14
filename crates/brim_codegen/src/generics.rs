use crate::codegen::CppCodegen;
use brim_hir::items::{HirGenericArgs, HirGenericKind, HirGenerics};

impl CppCodegen {
    pub fn generate_generics(&mut self, generics: &HirGenerics) {
        if generics.params.is_empty() {
            return;
        }

        let gens = {
            let mut gens = String::new();
            for param in &generics.params {
                match &param.kind {
                    HirGenericKind::Type { default } => {
                        gens.push_str(&format!("typename {}", param.name.to_string()));
                        if let Some(default) = default {
                            gens.push_str(&format!(
                                " = {}",
                                self.generate_ty(default.kind.clone())
                            ));
                        }
                    }
                    HirGenericKind::Const { ty, default } => {
                        gens.push_str(&format!(
                            "{} {}",
                            self.generate_ty(ty.kind.clone()),
                            param.name.to_string()
                        ));
                        if let Some(default) = default {
                            todo!("default const value");
                            // gens.push_str(&format!(" = {}", self.transform_const_expr(default.clone())));
                        }
                    }
                }
            }
            gens
        };
        self.code.add_line(&format!("template <{}>", gens));
    }

    pub fn generate_generic_args(&mut self, generics: &HirGenericArgs) -> String {
        if generics.params.is_empty() {
            return String::new();
        }

        let gens = {
            let mut gens = String::new();
            for param in &generics.params {
                gens.push_str(&format!("{}", self.generate_ty(param.ty.clone())));
            }
            gens
        };

        String::from(&format!("template <{}>", gens))
    }
}

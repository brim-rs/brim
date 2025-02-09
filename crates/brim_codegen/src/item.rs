use crate::codegen::CppCodegen;
use brim_hir::items::{HirItem, HirItemKind};

impl CppCodegen {
    pub fn generate_item(&mut self, item: HirItem) {
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

                let body_expr = self.hir.get_expr(decl.body.unwrap());
                let body_code = self.generate_expr(body_expr.clone());

                self.code.add_block(&block_name, |code| {
                    code.add_line(&body_code);
                });
            }
            _ => {}
        }
    }
}

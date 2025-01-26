use crate::codegen::CppCodegen;
use brim_hir::items::{HirItem, HirItemKind};

impl CppCodegen {
    pub fn generate_item(&mut self, item: HirItem) {
        match item.kind {
            HirItemKind::Fn(decl) => {
                let ret = self.generate_ty(decl.ret_type);

                self.generate_generics(&decl.sig.generics);
                let params = decl.sig.params.iter().map(|p| {
                    format!(
                        "{} {}",
                        self.generate_ty(p.ty.kind.clone()),
                        p.name.to_string()
                    )
                }).collect::<Vec<_>>().join(", ");
                self.code
                    .add_block(&format!("{} {}({})", ret, decl.sig.name.to_string(), params), |code| {
                        code.add_line("return 0;");
                    });
            }
            _ => {}
        }
    }
}

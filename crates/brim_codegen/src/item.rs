use crate::codegen::CppCodegen;
use brim_hir::items::{HirItem, HirItemKind};

impl CppCodegen {
    pub fn generate_item(&mut self, item: HirItem) {
        match item.kind {
            HirItemKind::Fn(decl) => {
                let ret = self.transform_ty(decl.ret_type);
                self.code
                    .add_line(&format!("{} {}(", ret, decl.sig.name.to_string()));
            }
            _ => {}
        }
    }
}

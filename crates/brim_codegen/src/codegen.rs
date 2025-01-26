use crate::CodeBuilder;
use brim_hir::transformer::{HirModule, HirModuleMap};

#[derive(Debug)]
pub struct CppCodegen {
    pub code: CodeBuilder,
    pub hir:HirModuleMap
}

impl CppCodegen {
    pub fn new(hir: HirModuleMap) -> Self {
        let mut code = CodeBuilder::new(4);

        for import in &["string", "vector", "cstdint"] {
            code.add_line(&format!("#include <{}>", import));
        }

        Self { code, hir }
    }

    pub fn generate(&mut self) {
        for module in self.hir.modules.clone() {
            self.generate_module(module);
        }
    }
    
    pub fn generate_module(&mut self, module: HirModule) {
        self.code
            .add_line(&format!("namespace module{} {{", module.mod_id.as_u32()));
        self.code.increase_indent();

        for item in module.items {
            self.generate_item(item);
        }

        self.code.decrease_indent();
        self.code.add_line("}");
    }
}

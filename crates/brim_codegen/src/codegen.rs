use crate::CodeBuilder;
use brim_hir::transformer::HirModule;

#[derive(Debug)]
pub struct CppCodegen {
    pub code: CodeBuilder,
}

impl CppCodegen {
    pub fn new() -> Self {
        let mut code = CodeBuilder::new(4);

        for import in &["string", "vector", "cstdint"] {
            code.add_line(&format!("#include <{}>", import));
        }

        Self { code }
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

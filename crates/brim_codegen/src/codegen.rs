use brim_hir::transformer::HirModule;
use crate::CodeBuilder;

#[derive(Debug)]
pub struct CppCodegen {
    pub code: CodeBuilder,
}

impl CppCodegen {
    pub fn new() -> Self {
        Self {
            code: CodeBuilder::new(4),
        }
    }

    pub fn add_include(&mut self, include: &str) {
        self.code.add_line(&format!("#include <{}>", include));
    }

    pub fn add_namespace<F>(&mut self, namespace: &str, body: F)
    where
        F: FnOnce(&mut CodeBuilder),
    {
        self.code.add_line(&format!("namespace {} {{", namespace));
        self.code.increase_indent();
        body(&mut self.code);
        self.code.decrease_indent();
        self.code.add_line("}");
    }
    
    /// Translates to namespace with name being `module{id}`
    pub fn generate_module(&mut self, module: HirModule) {
        self.add_namespace(&format!("module{}", module.mod_id.as_u32()), |code| {
            for item in module.items {
                
            }
        });
    }
    
}

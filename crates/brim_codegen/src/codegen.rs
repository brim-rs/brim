use crate::CodeBuilder;
use brim_hir::{
    Codegen, CompiledModules,
    expr::HirExpr,
    items::HirItem,
    stmts::HirStmt,
    transformer::{HirModule, HirModuleMap},
    ty::HirTyKind,
};
use brim_middle::ModuleId;
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug)]
pub struct CppCodegen {
    pub code: CodeBuilder,
    pub current_mod: ModuleId,
    pub hir: Option<HirModuleMap>,
    pub main_file: usize,
    pub modules: Vec<usize>,
    pub imports: Vec<String>,
}

impl CppCodegen {
    pub fn new(main_file: usize) -> Self {
        let mut code = CodeBuilder::new(4);

        Self {
            code,
            current_mod: ModuleId::from_usize(0),
            hir: None,
            main_file,
            modules: vec![],
            imports: ["string", "vector", "cstdint", "expected", "array"]
                .iter()
                .map(|s| s.to_string())
                .collect(),
        }
    }

    pub fn hir_mut(&mut self) -> &mut HirModuleMap {
        self.hir.as_mut().unwrap()
    }

    pub fn hir(&self) -> &HirModuleMap {
        self.hir.as_ref().unwrap()
    }

    pub fn add_standard_module(&mut self) {
        self.code.add_line("");

        for std_module in &self.imports {
            self.code.add_line(&format!("import <{}>;", std_module));
        }
        self.code.add_line("");
    }

    pub fn add_main(&mut self) {
        self.code.add_line("// main.cpp");
        self.code.add_line("import std;");

        // Import the main module
        self.code
            .add_line(&format!("import module{};", self.main_file));

        self.code.add_line("");
        self.code.add_line("int main() {");
        self.code.increase_indent();
        self.code
            .add_line(&format!("module{}::main();", self.main_file));
        self.code.add_line("return 0;");
        self.code.decrease_indent();
        self.code.add_line("}");
    }

    pub fn populate(&mut self, compiled: &CompiledModules) {
        for (_, project) in compiled.map.iter() {
            for module in &project.hir.modules {
                self.modules.push(module.mod_id.as_usize());
            }
        }
    }
}

impl Codegen for CppCodegen {
    fn generate(&mut self, compiled: &CompiledModules) {
        self.populate(compiled);

        for (_, project) in compiled.map.iter() {
            for module in &project.hir.modules {
                self.current_mod = module.mod_id;
                self.hir = Some(project.hir.clone());
                self.generate_module(module.clone(), compiled);
            }
        }
    }

    fn generate_module(&mut self, module: HirModule, compiled: &CompiledModules) {
        let mod_id = module.mod_id;

        self.code
            .add_line(&format!("\n\n// =========== module{}.cpp", mod_id.as_u32()));
        self.code
            .add_line(&format!("export module module{};", mod_id.as_u32()));
        self.code.add_line("");
        self.add_standard_module();
        self.code.add_line("");

        for dep_id in &self.modules {
            if *dep_id == mod_id.as_usize() {
                continue;
            }

            self.code.add_line(&format!("import module{};", dep_id));
        }
        self.code.add_line("");

        // Export module namespace
        self.code
            .add_line(&format!("export namespace module{} {{", mod_id.as_u32()));
        self.code.increase_indent();

        for item in module.items {
            let item = compiled.get_item(item).clone();
            // Call the trait method, not self
            Codegen::generate_item(self, item, compiled);
        }

        self.code.decrease_indent();
        self.code.add_line("}");
    }

    fn generate_item(&mut self, item: HirItem, compiled: &CompiledModules) {
        self.generate_item(item, compiled)
    }

    fn generate_expr(&mut self, expr: HirExpr) -> String {
        self.generate_expr(expr)
    }

    fn generate_stmt(&mut self, stmt: HirStmt) -> String {
        self.generate_stmt(stmt)
    }

    fn generate_ty(&mut self, ty: HirTyKind) -> String {
        self.generate_ty(ty)
    }
}

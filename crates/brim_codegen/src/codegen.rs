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
}

impl CppCodegen {
    pub fn new(main_file: usize) -> Self {
        let mut code = CodeBuilder::new(4);

        for import in &["string", "vector", "cstdint", "expected", "array"] {
            code.add_line(&format!("#include <{}>", import));
        }

        Self {
            code,
            current_mod: ModuleId::from_usize(0),
            hir: None,
            main_file,
        }
    }

    pub fn hir_mut(&mut self) -> &mut HirModuleMap {
        self.hir.as_mut().unwrap()
    }

    pub fn hir(&self) -> &HirModuleMap {
        self.hir.as_ref().unwrap()
    }

    pub fn add_main(&mut self) {
        self.code.add_line("int main() {");
        self.code.increase_indent();
        self.code.add_line(&format!("module{}::main();", self.main_file));
        self.code.add_line("return 0;");
        self.code.decrease_indent();
        self.code.add_line("}");
    }
}

impl Codegen for CppCodegen {
    fn generate(&mut self, compiled: &CompiledModules) {
        for (_, project) in &compiled.map {
            for module in &project.hir.modules {
                self.hir = Some(project.hir.clone());

                self.current_mod = module.mod_id.clone();
                self.generate_module(module.clone(), compiled);
            }
        }

        self.add_main();
    }

    fn generate_module(&mut self, module: HirModule, compiled: &CompiledModules) {
        self.code
            .add_line(&format!("namespace module{} {{", module.mod_id.as_u32()));
        self.code.increase_indent();

        for item in module.items {
            let item = compiled.get_item(item).clone();

            self.generate_item(item, compiled);
        }

        self.code.decrease_indent();
        self.code.add_line("}");
    }

    fn generate_item(&mut self, item: HirItem, compiled: &CompiledModules) {
        self.generate_item(item, compiled);
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

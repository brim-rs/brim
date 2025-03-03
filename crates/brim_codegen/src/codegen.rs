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
    pub hir: HirModuleMap,
    pub current_mod: ModuleId,
    pub compiled: CompiledModules,
}

impl CppCodegen {
    pub fn new(hir: HirModuleMap, compiled: CompiledModules) -> Self {
        let mut code = CodeBuilder::new(4);

        for import in &["string", "vector", "cstdint", "expected", "array"] {
            code.add_line(&format!("#include <{}>", import));
        }

        Self {
            code,
            hir,
            current_mod: ModuleId::from_usize(0),
            compiled,
        }
    }

    pub fn add_main(&mut self) {
        self.code.add_line("int main() {");
        self.code.increase_indent();
        self.code.add_line("module0::main();");
        self.code.add_line("return 0;");
        self.code.decrease_indent();
        self.code.add_line("}");
    }
}

impl Codegen for CppCodegen {
    fn generate(&mut self) {
        let mut graph: HashMap<ModuleId, HashSet<ModuleId>> = HashMap::new();
        let mut indegree: HashMap<ModuleId, usize> = HashMap::new();
        let mut module_map: HashMap<ModuleId, HirModule> = HashMap::new();

        for module in &self.hir.modules {
            module_map.insert(module.mod_id.clone(), module.clone());
            indegree.insert(module.mod_id.clone(), 0);
        }

        for module in &self.hir.modules {
            for import in &module.imports {
                graph
                    .entry(import.module.clone())
                    .or_default()
                    .insert(module.mod_id.clone());
                *indegree.entry(module.mod_id.clone()).or_default() += 1;
            }
        }

        // Perform a topological sort using Kahn's Algorithm
        let mut queue: VecDeque<ModuleId> = indegree
            .iter()
            .filter_map(|(mod_id, &deg)| if deg == 0 { Some(mod_id.clone()) } else { None })
            .collect();

        let mut sorted_modules = Vec::new();

        while let Some(mod_id) = queue.pop_front() {
            if let Some(module) = module_map.get(&mod_id) {
                sorted_modules.push(module.clone());
            }

            if let Some(dependencies) = graph.get(&mod_id) {
                for dependent in dependencies {
                    let entry = indegree.get_mut(dependent).unwrap();
                    *entry -= 1;
                    if *entry == 0 {
                        queue.push_back(dependent.clone());
                    }
                }
            }
        }

        if sorted_modules.len() != self.hir.modules.len() {
            panic!("Circular dependency detected in modules");
        }

        self.hir.modules = sorted_modules.clone();

        for module in sorted_modules {
            self.current_mod = module.mod_id.clone();
            self.generate_module(module);
        }

        self.add_main();
    }

    fn generate_module(&mut self, module: HirModule) {
        self.code
            .add_line(&format!("namespace module{} {{", module.mod_id.as_u32()));
        self.code.increase_indent();

        for item in module.items {
            let item = self.compiled.get_item(item).clone();

            self.generate_item(item);
        }

        self.code.decrease_indent();
        self.code.add_line("}");
    }

    fn generate_item(&mut self, item: HirItem) {
        self.generate_item(item);
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

use crate::{CodeBuilder, item::sort_items_by_module};
use brim_ast::{
    ItemId,
    item::{FunctionContext, Ident},
};
use brim_hir::{
    Codegen, CompiledModules,
    expr::HirExpr,
    items::{HirGenerics, HirItem, HirItemKind, HirStruct},
    stmts::HirStmt,
    transformer::{HirModule, HirModuleMap},
    ty::HirTyKind,
};
use brim_middle::{GlobalSymbol, ModuleId};
use std::{
    any::Any,
    collections::{HashMap, HashSet},
    path::PathBuf,
};
use tracing::debug;

#[derive(Debug)]
pub struct CppCodegen {
    pub code: CodeBuilder,
    pub current_mod: ModuleId,
    pub hir: Option<HirModuleMap>,
    pub main_file: usize,
    pub modules: Vec<usize>,
    pub imports: Vec<String>,
    pub add_prefix: bool,
    pub compiled: CompiledModules,
    pub parent_item: Option<(Ident, HirGenerics)>,
    pub items_order: HashMap<ModuleId, Vec<ItemId>>,
}

#[derive(Debug)]
pub struct ModuleDependencyResolver {
    module_dependencies: HashMap<ModuleId, HashSet<ModuleId>>,
    module_imports: HashMap<ModuleId, Vec<PathBuf>>,
    processed_modules: HashSet<ModuleId>,
    hirs: HashMap<ModuleId, HirModuleMap>,
}

impl ModuleDependencyResolver {
    fn new(compiled: &CompiledModules) -> Self {
        let mut resolver = Self {
            module_dependencies: HashMap::new(),
            module_imports: HashMap::new(),
            processed_modules: HashSet::new(),
            hirs: HashMap::new(),
        };

        resolver.build_dependency_graph(compiled);

        resolver
    }

    fn build_dependency_graph(&mut self, compiled: &CompiledModules) {
        for (_, project) in compiled.map.iter() {
            for module in &project.hir.modules {
                let module_id = module.mod_id;
                let mut dependencies = HashSet::new();
                let mut module_imports = Vec::new();

                for item_id in &module.items {
                    let item = compiled.get_item(*item_id).clone();

                    if let HirItemKind::Use(use_stmt) = &item.kind {
                        module_imports.push(use_stmt.resolved_path.clone());

                        if let Some(imported_mod_id) =
                            self.find_module_id_for_path(compiled, &use_stmt.resolved_path)
                        {
                            dependencies.insert(imported_mod_id);
                        }
                    }
                }

                self.module_dependencies.insert(module_id, dependencies);
                self.module_imports.insert(module_id, module_imports);
                self.hirs.insert(module_id, project.hir.clone());
            }
        }
    }

    fn find_module_id_for_path(
        &self,
        compiled: &CompiledModules,
        path: &PathBuf,
    ) -> Option<ModuleId> {
        for (_, project) in compiled.map.iter() {
            for module in &project.hir.modules {
                if module.path == *path {
                    return Some(module.mod_id);
                }
            }
        }
        None
    }

    fn determine_generation_order(&mut self) -> Vec<ModuleId> {
        let mut generation_order = Vec::new();
        let mut remaining_modules: HashSet<ModuleId> =
            self.module_dependencies.keys().cloned().collect();

        while !remaining_modules.is_empty() {
            let modules_ready_for_generation: Vec<ModuleId> = remaining_modules
                .iter()
                .filter(|&module_id| {
                    let deps = self
                        .module_dependencies
                        .get(module_id)
                        .cloned()
                        .unwrap_or(HashSet::new());

                    deps.is_empty() || deps.iter().all(|dep| self.processed_modules.contains(dep))
                })
                .cloned()
                .collect();

            if modules_ready_for_generation.is_empty() {
                panic!(
                    "Circular module dependencies detected: {:?}",
                    remaining_modules
                        .iter()
                        .map(|&mod_id| format!("Module {}", mod_id.as_u32()))
                        .collect::<Vec<_>>()
                );
            }

            for module_id in modules_ready_for_generation {
                generation_order.push(module_id);
                self.processed_modules.insert(module_id);
                remaining_modules.remove(&module_id);

                for deps in self.module_dependencies.values_mut() {
                    deps.remove(&module_id);
                }
            }
        }

        generation_order
    }

    fn print_module_dependencies(&self) {
        debug!("Module Dependencies:");
        for (module_id, dependencies) in &self.module_dependencies {
            debug!(
                "Module {}: Depends on {:?}",
                module_id.as_u32(),
                dependencies.iter().map(|d| d.as_u32()).collect::<Vec<_>>()
            );
        }
    }
}

impl CppCodegen {
    pub fn new(main_file: usize, compiled: CompiledModules) -> Self {
        let code = CodeBuilder::new(4);

        let sorted_modules = sort_items_by_module(&compiled.item_relations);

        Self {
            code,
            current_mod: ModuleId::from_usize(0),
            hir: None,
            main_file,
            modules: vec![],
            imports: [
                "string",
                "vector",
                "cstdint",
                "expected",
                "any",
                "source_location",
                "cstdlib",
                "sstream",
                "iostream",
                "optional",
                "typeinfo",
                "typeindex",
                "cstring",
                "cstddef",
                "span",
                "variant",
            ]
            .iter()
            .map(|s| s.to_string())
            .collect(),
            add_prefix: true,
            compiled,
            parent_item: None,
            items_order: sorted_modules,
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
            self.code.add_line(&format!("#include <{}>;", std_module));
        }
        self.code.add_line("");
    }

    pub fn add_main(&mut self) {
        self.code.add_line("// main.cpp");

        self.code.add_line("");
        self.code.add_line("int main() {");
        self.code.increase_indent();
        self.code
            .add_line(&format!("module{}::brim_main();", self.main_file));
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

    pub fn add_injects(&mut self) {
        self.code.add_line(include_str!("./injects.cpp"));
    }
}

impl Codegen for CppCodegen {
    fn generate(&mut self, compiled: &CompiledModules) {
        let mut resolver = ModuleDependencyResolver::new(compiled);

        resolver.print_module_dependencies();

        let generation_order = resolver.determine_generation_order();

        self.populate(compiled);
        self.add_standard_module();
        self.add_injects();

        for module_id in generation_order {
            let module = compiled
                .map
                .values()
                .flat_map(|project| &project.hir.modules)
                .find(|m| m.mod_id == module_id)
                .expect("Module not found");

            self.current_mod = module.mod_id;
            self.hir = Some(resolver.hirs.get(&module_id).unwrap().clone());
            self.generate_module(module.clone(), compiled);
        }

        self.add_main();
    }

    fn generate_module(&mut self, module: HirModule, compiled: &CompiledModules) {
        let mod_id = module.mod_id;

        self.code
            .add_line(&format!("\n\n// =========== module{}", mod_id.as_u32()));
        self.code.add_line("");

        self.code
            .add_line(&format!("namespace module{} {{", mod_id.as_u32()));
        self.code.increase_indent();

        let order = self.items_order.get(&mod_id);

        if let Some(mut order) = order.cloned() {
            for item in &module.items {
                if !order.contains(item) {
                    order.push(*item);
                }
            }
            for item in order {
                let item = compiled.get_item(item).clone();

                match &item.kind {
                    HirItemKind::Fn(f) => {
                        if f.ctx == FunctionContext::Extern {
                            continue;
                        }
                    }
                    _ => {}
                }

                self.generate_item(item, compiled);
            }
        } else {
            for item in module.items {
                let item = compiled.get_item(item).clone();
                self.generate_item(item, compiled);
            }
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

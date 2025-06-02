use crate::CodeBuilder;
use brim_ast::{
    ItemId,
    item::{ExternBlock, FunctionContext},
};
use brim_hir::{
    Codegen, MainContext,
    expr::HirExpr,
    items::{HirExternBlock, HirItem, HirItemKind},
    stmts::HirStmt,
    transformer::{HirModule, HirModuleMap},
    ty::HirTyKind,
};
use brim_middle::{GlobalSymbol, ModuleId};
use std::{
    cmp::Reverse,
    collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet},
    path::PathBuf,
};
use tracing::debug;

#[derive(Debug)]
pub struct DefaultCodegen {
    pub code: CodeBuilder,
    pub current_mod: ModuleId,
    pub hir: Option<HirModuleMap>,
    pub main_file: usize,
    pub modules: Vec<usize>,
    pub main_ctx: MainContext,
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
    fn new(main_ctx: &MainContext) -> Self {
        let mut resolver = Self {
            module_dependencies: HashMap::new(),
            module_imports: HashMap::new(),
            processed_modules: HashSet::new(),
            hirs: HashMap::new(),
        };

        resolver.build_dependency_graph(main_ctx);

        resolver
    }

    fn build_dependency_graph(&mut self, main_ctx: &MainContext) {
        for project in main_ctx.map.values() {
            for module in project.hir.modules() {
                let module_id = module.mod_id;
                let mut dependencies = HashSet::new();
                let mut module_imports = Vec::new();

                for item_id in &module.items {
                    let item = main_ctx.get_item(*item_id).clone();

                    if let HirItemKind::Use(use_stmt) = &item.kind {
                        module_imports.push(use_stmt.resolved_path.clone());

                        if let Some(imported_mod_id) =
                            self.find_module_id_for_path(main_ctx, &use_stmt.resolved_path)
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

    fn find_module_id_for_path(&self, main_ctx: &MainContext, path: &PathBuf) -> Option<ModuleId> {
        for project in main_ctx.map.values() {
            for module in project.hir.modules() {
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
            self.module_dependencies.keys().copied().collect();

        while !remaining_modules.is_empty() {
            let modules_ready_for_generation: Vec<ModuleId> = remaining_modules
                .iter()
                .filter(|&module_id| {
                    let deps =
                        self.module_dependencies.get(module_id).cloned().unwrap_or(HashSet::new());

                    deps.is_empty() || deps.iter().all(|dep| self.processed_modules.contains(dep))
                })
                .copied()
                .collect();

            assert!(
                !modules_ready_for_generation.is_empty(),
                "Circular module dependencies detected: {:?}",
                remaining_modules
                    .iter()
                    .map(|&mod_id| format!("Module {}", mod_id.as_u32()))
                    .collect::<Vec<_>>()
            );

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

impl DefaultCodegen {
    pub fn new(main_file: usize, main_ctx: MainContext) -> Self {
        let code = CodeBuilder::new(4);

        let sorted_modules = sort_items_by_module(&main_ctx.item_relations);

        Self {
            code,
            current_mod: ModuleId::from_usize(0),
            hir: None,
            main_file,
            modules: vec![],
            main_ctx,
            items_order: sorted_modules,
        }
    }

    pub fn hir_mut(&mut self) -> &mut HirModuleMap {
        self.hir.as_mut().unwrap()
    }

    pub fn hir(&self) -> &HirModuleMap {
        self.hir.as_ref().unwrap()
    }

    pub fn populate(&mut self, main_ctx: &MainContext) {
        for project in main_ctx.map.values() {
            for module in project.hir.modules() {
                self.modules.push(module.mod_id.as_usize());
            }
        }
    }
}

impl Codegen for DefaultCodegen {
    fn generate(&mut self, main_ctx: &MainContext) {
        let mut resolver = ModuleDependencyResolver::new(main_ctx);

        resolver.print_module_dependencies();

        let generation_order = resolver.determine_generation_order();
        self.populate(main_ctx);

        for module_id in generation_order {
            let module = main_ctx
                .map
                .values()
                .flat_map(|project| project.hir.modules())
                .find(|m| m.mod_id == module_id)
                .expect("Module not found");

            self.current_mod = module.mod_id;
            self.hir = Some(resolver.hirs[&module_id].clone());
            self.generate_module(module.clone(), main_ctx);
        }
    }

    fn generate_module(&mut self, module: HirModule, main_ctx: &MainContext) {
        let mod_id = module.mod_id;

        let mut items = BTreeMap::new();

        for &item_id in &module.items {
            let item = main_ctx.get_item(item_id).clone();

            items.insert(item.ident, item.clone());
        }

        let ordered_item_ids = self.items_order.get(&mod_id).cloned();

        if let Some(ordered_ids) = ordered_item_ids {
            let mut processed_items = HashSet::new();

            for item_id in ordered_ids {
                let item = main_ctx.get_item(item_id);

                if !processed_items.insert(item_id) {
                    continue;
                }

                match &item.kind {
                    HirItemKind::EnumVariant(_)
                    | HirItemKind::Namespace(_)
                    | HirItemKind::TypeAlias(_) => continue,
                    HirItemKind::Fn(f) if f.ctx == FunctionContext::Extern => continue,
                    _ => {}
                }

                if let Some(item) = items.remove(&item.ident) {
                    self.generate_item(item, main_ctx);
                }
            }

            for (_, item) in items {
                match &item.kind {
                    HirItemKind::EnumVariant(_)
                    | HirItemKind::Namespace(_)
                    | HirItemKind::TypeAlias(_) => continue,
                    HirItemKind::Fn(f) if f.ctx == FunctionContext::Extern => continue,
                    _ => {}
                }

                self.generate_item(item, main_ctx);
            }
        } else {
            for (_, item) in items {
                match &item.kind {
                    HirItemKind::EnumVariant(_)
                    | HirItemKind::Namespace(_)
                    | HirItemKind::TypeAlias(_) => continue,
                    HirItemKind::Fn(f) if f.ctx == FunctionContext::Extern => continue,
                    _ => {}
                }

                self.generate_item(item, main_ctx);
            }
        }
    }

    fn generate_item(&mut self, item: HirItem, main_ctx: &MainContext) {}

    fn generate_expr(&mut self, expr: HirExpr) -> String {
        String::new()
    }

    fn generate_stmt(&mut self, stmt: HirStmt) -> String {
        String::new()
    }

    fn generate_ty(&mut self, ty: HirTyKind) -> String {
        String::new()
    }
}

pub fn sort_items_by_module(
    item_relations: &HashMap<GlobalSymbol, Vec<GlobalSymbol>>,
) -> HashMap<ModuleId, Vec<ItemId>> {
    let mut module_items: BTreeMap<ModuleId, BTreeSet<ItemId>> = BTreeMap::new();
    let mut all_items: BTreeSet<GlobalSymbol> = BTreeSet::new();

    let mut dependency_map: HashMap<GlobalSymbol, Vec<GlobalSymbol>> = HashMap::new();

    for (item, relations) in item_relations {
        all_items.insert(item.clone());

        for related_item in relations {
            all_items.insert(related_item.clone());

            dependency_map.entry(related_item.clone()).or_default().push(item.clone());
        }
    }

    for item in &all_items {
        module_items.entry(item.id.mod_id).or_default().insert(item.id.item_id);
    }

    let mut module_graphs: BTreeMap<ModuleId, BTreeMap<ItemId, Vec<ItemId>>> = BTreeMap::new();

    for (item, dependencies) in &dependency_map {
        let mod_id = item.id.mod_id;
        let item_id = item.id.item_id;

        module_graphs.entry(mod_id).or_default();

        let same_module_deps: Vec<ItemId> = dependencies
            .iter()
            .filter(|dep| dep.id.mod_id == mod_id)
            .map(|dep| dep.id.item_id)
            .collect();

        module_graphs.get_mut(&mod_id).unwrap().insert(item_id, same_module_deps);
    }

    for (mod_id, items) in &module_items {
        let graph_entry = module_graphs.entry(*mod_id).or_default();

        for &item_id in items {
            graph_entry.entry(item_id).or_default();
        }
    }

    let mut result: HashMap<ModuleId, Vec<ItemId>> = HashMap::new();

    for (mod_id, items) in &module_items {
        let temp = BTreeMap::new();
        let graph = module_graphs.get(mod_id).unwrap_or(&temp);
        let sorted_items = topological_sort(items, graph);
        result.insert(*mod_id, sorted_items);
    }

    result
}

pub fn topological_sort(
    items: &BTreeSet<ItemId>,
    graph: &BTreeMap<ItemId, Vec<ItemId>>,
) -> Vec<ItemId> {
    let mut indegree: BTreeMap<ItemId, usize> = BTreeMap::new();
    let mut adj_list: BTreeMap<ItemId, Vec<ItemId>> = BTreeMap::new();

    for item in items {
        indegree.insert(*item, 0);
        adj_list.insert(*item, Vec::new());
    }

    for (item, deps) in graph {
        for dep in deps {
            if items.contains(dep) {
                adj_list.entry(*item).or_default().push(*dep);
                *indegree.entry(*dep).or_default() += 1;
            }
        }
    }

    let mut queue: BinaryHeap<Reverse<ItemId>> =
        indegree.iter().filter(|&(_, &count)| count == 0).map(|(item, _)| Reverse(*item)).collect();

    let mut result = Vec::new();

    while let Some(Reverse(item)) = queue.pop() {
        result.push(item);

        if let Some(adjacent) = adj_list.get(&item) {
            for adj in adjacent {
                if let Some(count) = indegree.get_mut(adj) {
                    *count -= 1;
                    if *count == 0 {
                        queue.push(Reverse(*adj));
                    }
                }
            }
        }
    }

    if result.len() != items.len() {
        for item in items {
            if !result.contains(item) {
                result.push(*item);
            }
        }
    }

    result
}

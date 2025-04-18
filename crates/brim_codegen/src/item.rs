use crate::codegen::CppCodegen;
use brim_ast::ItemId;
use brim_hir::{
    MainContext,
    items::{HirItem, HirItemKind},
};
use brim_middle::{GlobalSymbol, ModuleId};
use std::collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap};

impl CppCodegen {
    pub fn generate_item(&mut self, item: HirItem, main_ctx: &MainContext) {
        match item.kind {
            HirItemKind::Fn(ref decl) => {
                self.code.add_line(&format!("// Generated item with id {}", item.id.as_usize()));

                let ret = self.generate_ty(decl.sig.return_type.clone());

                let generics = &decl.sig.generics;
                self.generate_generics(generics);

                let params = &decl.sig.params.params;
                let params = params
                    .iter()
                    .map(|p| format!("{} brim_{}", self.generate_ty(p.ty.kind.clone()), p.name))
                    .collect::<Vec<_>>()
                    .join(", ");

                let name = if self.is_external.is_none() {
                    format!("brim_{}", decl.sig.name)
                } else {
                    decl.sig.name.to_string()
                };
                let before = if let Some(external) = &self.is_external {
                    if let Some(abi) = external.abi {
                        format!("extern \"{abi}\" __declspec(dllimport)")
                    } else {
                        "__declspec(dllimport)".to_string()
                    }
                } else {
                    "".to_string()
                };
                let callconv = if let Some(attr) = item.has_attr("callconv") {
                    if let Some(conv) = attr.has_first_string_argument() {
                        if conv == "winapi" {
                            " __stdcall"
                        } else if conv == "fastcall" {
                            " __fastcall"
                        } else if conv == "cdecl" {
                            " __cdecl"
                        } else {
                            panic!("Invalid callconv attribute");
                        }
                        .to_string()
                    } else {
                        panic!("Invalid callconv attribute");
                    }
                } else {
                    "".to_string()
                };
                let is_static = if self.generate_static { "static" } else { "" };
                let block_name = format!("{before} {is_static} {ret} {callconv} {name}({params})");

                if let Some(body) = &decl.body {
                    let body_code = self.generate_expr(body.clone());

                    self.code.add_block(&block_name, |code| {
                        code.add_line_no_indent(&body_code);
                    });
                } else {
                    self.code.add_line(&format!("{block_name};"));
                };
            }
            HirItemKind::Struct(mut s) => {
                self.code.add_line(&format!("// Generated item with id {}", item.id.as_usize()));

                self.generate_generics(&s.generics);
                let name = if self.is_external.is_none() {
                    format!("brim_{}", s.ident.name)
                } else {
                    s.ident.name.to_string()
                };
                self.code.add_line(&format!("struct {name} {{"));
                self.code.increase_indent();

                for field in s.fields.clone() {
                    let ty = self.generate_ty(field.ty.clone());

                    self.code.add_line(&format!("{} {};", ty, field.ident));
                }

                for (ident, id) in s.items.clone() {
                    let item = self.main_ctx.get_item(id);

                    if let Some(func) = item.as_fn_safe() {
                        if !func.is_static() {
                            self.generate_item(item.clone(), main_ctx);

                            s.items.remove(&ident);
                        }
                    }
                }

                self.generate_static = true;
                for (_, id) in s.items {
                    let item = self.main_ctx.get_item(id);

                    self.generate_item(item.clone(), main_ctx);
                }
                self.generate_static = false;

                self.code.decrease_indent();
                self.code.add_line("};");
            }
            HirItemKind::External(external) => {
                self.is_external = Some(external.clone());
                for item in external.items.clone() {
                    let item = main_ctx.get_item(item).clone();
                    self.generate_item(item.clone(), main_ctx);
                }
                self.is_external = None;

                for item in external.items {
                    let item = main_ctx.get_item(item).clone();
                    self.generate_wrapper_item(item);
                }
            }
            HirItemKind::Enum(mut e) => {
                self.code.add_line(&format!("// Generated item with id {}", item.id.as_usize()));

                self.generate_generics(&e.generics);
                let name = if self.is_external.is_none() {
                    format!("brim_{}", e.ident.name)
                } else {
                    e.ident.name.to_string()
                };
                let mut variant_names = vec![];

                self.code.add_line(&format!("struct {name} {{"));
                self.code.increase_indent();

                for variant in &e.variants {
                    let variant_name = format!("brim_{}", variant.ident);
                    variant_names.push(variant_name.clone());

                    self.code.add_line(&format!("struct {variant_name} {{"));
                    self.code.increase_indent();

                    for (i, field) in variant.fields.iter().enumerate() {
                        let ty = self.generate_ty(field.ty.clone());
                        self.code.add_line(&format!("{ty} field_{i};"));
                    }

                    let params: Vec<String> = variant
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| format!("{} f{}", self.generate_ty(field.ty.clone()), i))
                        .collect();

                    let init_list: Vec<String> =
                        (0..variant.fields.len()).map(|i| format!("field_{i}(f{i})")).collect();

                    if variant.fields.is_empty() {
                        self.code.add_line(&format!("{variant_name}() = default;"));
                    } else {
                        self.code.add_line(&format!(
                            "{}({}) : {} {{}}",
                            variant_name,
                            params.join(", "),
                            init_list.join(", ")
                        ));
                    }

                    self.code.decrease_indent();
                    self.code.add_line("};");
                }

                self.code.add_line("std::variant<");
                for (index, variant_name) in variant_names.iter().enumerate() {
                    self.code.add_line(&format!(
                        "{}{}",
                        variant_name,
                        if index == variant_names.len() - 1 { "" } else { "," }
                    ));
                }
                self.code.add_line("> value;");

                self.code.add_line(&format!("{name}() = default;"));

                self.code.add_line(&format!(
                    "{name}(const {name}& other) : value(std::visit([](const auto& v) {{ return decltype(value)(v); }}, other.value)) {{}}"
                ));

                self.code.add_line(&format!(
                    "{name}({name}&& other) noexcept : value(std::move(other.value)) {{}}"
                ));

                for variant_name in &variant_names {
                    self.code
                        .add_line(&format!("{name}({variant_name} v) : value(std::move(v)) {{}}"));
                }

                for (ident, id) in e.items.clone() {
                    let item = self.main_ctx.get_item(id);

                    if let Some(func) = item.as_fn_safe() {
                        if !func.is_static() {
                            self.generate_item(item.clone(), main_ctx);

                            e.items.remove(&ident);
                        }
                    }
                }

                self.generate_static = true;
                for (_, id) in e.items {
                    let item = self.main_ctx.get_item(id);

                    self.generate_item(item.clone(), main_ctx);
                }
                self.generate_static = false;

                self.code.decrease_indent();
                self.code.add_line("};");
            }
            _ => {}
        }
    }

    pub fn generate_wrapper_item(&mut self, item: HirItem) {
        if let HirItemKind::Fn(decl) = item.kind {
            let ret = self.generate_ty(decl.sig.return_type);
            self.generate_generics(&decl.sig.generics);

            let params = decl
                .sig
                .params
                .params
                .iter()
                .map(|p| format!("{} {}", self.generate_ty(p.ty.kind.clone()), p.name))
                .collect::<Vec<_>>()
                .join(", ");

            let original_name = decl.sig.name.to_string();
            let wrapper_name = format!("brim_{original_name}");
            let block_name = format!("inline {ret} {wrapper_name}({params})");

            self.code.add_block(&block_name, |code| {
                let param_names = decl
                    .sig
                    .params
                    .params
                    .iter()
                    .map(|p| p.name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                code.add_line(&format!("return {original_name}({param_names});"));
            });
        }
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

    let mut queue: BinaryHeap<std::cmp::Reverse<ItemId>> = indegree
        .iter()
        .filter(|&(_, &count)| count == 0)
        .map(|(item, _)| std::cmp::Reverse(*item))
        .collect();

    let mut result = Vec::new();

    while let Some(std::cmp::Reverse(item)) = queue.pop() {
        result.push(item);

        if let Some(adjacent) = adj_list.get(&item) {
            for adj in adjacent {
                if let Some(count) = indegree.get_mut(adj) {
                    *count -= 1;
                    if *count == 0 {
                        queue.push(std::cmp::Reverse(*adj));
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

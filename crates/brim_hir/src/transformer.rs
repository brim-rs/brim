use crate::{
    HirId,
    expr::HirExpr,
    items::{HirFn, HirFnSig, HirGenericParam, HirGenerics, HirItem, HirItemKind, HirParam},
    stmts::HirStmt,
    ty::HirTy,
};
use brim_ast::{
    NodeId,
    item::{Item, ItemKind},
};
use brim_ctx::{
    ModuleId,
    modules::{Module, ModuleMap},
};
use std::{collections::HashMap, path::PathBuf};

#[derive(Clone, Debug)]
pub struct LocId {
    pub id: HirId,
    pub module: ModuleId,
}

pub fn transform_module(map: ModuleMap) -> HirModuleMap {
    let mut transformer = Transformer::new();
    transformer.transform_modules(map)
}

#[derive(Debug)]
pub struct HirModuleMap {
    pub modules: Vec<HirModule>,
}

#[derive(Clone, Debug)]
pub enum StoredHirItem {
    Item(HirItem),
    Stmt(HirStmt),
    Expr(HirExpr),
}

#[derive(Clone, Debug)]
pub struct HirModule {
    /// In hir we no longer use file ids, we use module ids.
    pub mod_id: ModuleId,
    pub items: Vec<HirItem>,
    pub hir_items: HashMap<HirId, StoredHirItem>,
    // Not sure if this will be needed
    path: PathBuf,
}

#[derive(Debug)]
pub struct Transformer {
    pub modules: Vec<HirModule>,
}

impl Transformer {
    pub fn new() -> Self {
        Self { modules: vec![] }
    }

    pub fn transform_modules(&mut self, module: ModuleMap) -> HirModuleMap {
        for module in module.modules {
            let hir_module = self.transform_module(module);
            self.modules.push(hir_module);
        }

        HirModuleMap {
            modules: self.modules.clone(),
        }
    }

    pub fn transform_module(&mut self, module: Module) -> HirModule {
        let items = module
            .barrel
            .items
            .iter()
            .map(|item| self.transform_item(item))
            .collect();
        HirModule {
            mod_id: ModuleId::from_usize(module.barrel.file_id),
            items,
            hir_items: Default::default(),
            path: module.path,
        }
    }

    pub fn transform_item(&mut self, item: Item) -> HirItem {
        let hir_item_kind = match item.kind.clone() {
            ItemKind::Fn(f_decl) => HirItemKind::Fn(HirFn {
                sig: HirFnSig {
                    constant: f_decl.sig.constant.as_bool(),
                    name: f_decl.sig.name,
                    return_type: if let Some(ty) = f_decl.sig.return_type {
                        Some(self.transform_ty(ty))
                    } else {
                        Self
                    },
                    params: f_decl
                        .sig
                        .params
                        .iter()
                        .map(|param| HirParam {
                            id: HirId::from_u32(param.id.as_u32()),
                            span: param.span,
                            name: param.name,
                            ty: self.transform_ty(param.ty.clone()),
                        })
                        .collect(),
                    generics: HirGenerics {
                        params: f_decl
                            .generics
                            .params
                            .iter()
                            .map(|param| HirGenericParam {
                                id: HirId::from_u32(param.id.as_u32()),
                                name: param.ident,
                                kind: self.hir_generic_kind(param.kind.clone()),
                            })
                            .collect(),
                        span: f_decl.generics.span,
                    },
                    span: f_decl.sig.span,
                },
            }),
            _ => {}
        };

        HirItem {
            id: self.nid_to_hid(item.id),
            span: item.span,
            ident: item.ident,
            kind: hir_item_kind,
            is_public: item.vis.kind.is_public(),
        }
    }

    /// Converts NodeId to HirId
    pub fn nid_to_hid(&self, node_id: NodeId) -> HirId {
        HirId::from_u32(node_id.as_u32())
    }
}

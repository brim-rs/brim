use brim_ctx::ModuleId;
use brim_ctx::modules::ModuleMap;
use crate::HirId;
use crate::items::HirItem;

#[derive(Clone, Debug)]
pub struct LocId {
    pub id: HirId,
    pub module: ModuleId,
}

pub fn transform_module(module: ModuleMap) {
    // let mut transformer = Transformer::new();
    // transformer.transform_module(module)
}

#[derive(Debug)]
pub struct HirModuleMap {
    pub modules: Vec<HirModule>,
}

#[derive(Debug)]
pub struct HirModule {
    /// In hir we no longer use file ids, we use module ids.
    pub mod_id: ModuleId,
    pub items: Vec<HirItem>,

}
use crate::transformer::{HirModuleMap};

#[derive(Debug)]
pub struct TypeInference {
    pub hir: HirModuleMap,
}
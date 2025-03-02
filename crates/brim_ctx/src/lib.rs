#![feature(let_chains)]

use brim_ast::ItemId;
use brim_config::toml::Config;
use brim_hir::{items::HirItem, transformer::HirModuleMap};
use brim_middle::SymbolTable;
use std::collections::HashMap;

pub mod compiler;
pub mod diag_ctx;
pub mod errors;
pub mod name;
pub mod validator;

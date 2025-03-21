#![feature(let_chains)]

use brim_ast::{ItemId, item::Item};
use std::collections::HashMap;

pub mod compiler;
pub mod diag_ctx;
pub mod errors;
pub mod name;
pub mod validator;

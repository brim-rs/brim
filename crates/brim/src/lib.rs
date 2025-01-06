extern crate core;

pub mod compiler;
pub mod diag_ctx;
pub mod session;

use anyhow::bail;
pub use brim_ast::*;
pub use brim_config::*;
pub use brim_diag_macro::*;
pub use brim_diagnostics::*;
pub use brim_fs::*;
pub use brim_lexer::*;
pub use brim_shell::*;
pub use brim_span::*;

#[macro_export]
macro_rules! box_diag {
    ($diag:expr) => {
        return Err(Box::new($diag))
    };
}
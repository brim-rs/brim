pub mod reporting;

use crate::diagnostic::{Diagnostic, ToDiagnostic};
pub use reporting::*;
use std::fmt::Debug;

#[macro_export]
macro_rules! box_diag {
    ($diag:expr) => {
        return Err(Box::new($diag))
    };
    (@opt $diag:expr) => {
        return Some(Box::new($diag))
    };
}

#[macro_export]
macro_rules! diag_opt {
    ($ctx:expr, $diag:expr) => {
        if let Some(diag) = $diag {
            $ctx.emit_inner(diag);
        }
    };
}

pub type OptionalDiag<'a> = Option<Box<dyn ToDiagnostic<'a> + 'a>>;

/// Struct to store diagnostics to be later emitted by the main diagnostic context
pub struct TemporaryDiagnosticContext<'diags> {
    pub diags: Vec<Diagnostic<'diags, usize>>,
}

impl<'diags> TemporaryDiagnosticContext<'diags> {
    pub fn new() -> Self {
        Self { diags: vec![] }
    }

    pub fn emit(&mut self, diag: Box<dyn ToDiagnostic<'diags> + 'diags>) {
        self.diags.push(diag.to_diagnostic());
    }
}

impl Debug for TemporaryDiagnosticContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TemporaryDiagnosticContext")
            .field("diags", &self.diags.len())
            .finish()
    }
}

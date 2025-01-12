pub mod reporting;

use std::fmt::Debug;
pub use reporting::*;
use crate::diagnostic::{Diagnostic, ToDiagnostic};

#[macro_export]
macro_rules! box_diag {
    ($diag:expr) => {
        return Err(Box::new($diag))
    };
}

/// Struct to store diagnostics to be later emitted by the main diagnostic context
pub struct TemporaryDiagnosticContext<'diags> {
    pub diags: Vec<Diagnostic<'diags, usize>>,
}

impl<'diags> TemporaryDiagnosticContext<'diags> {
    pub fn new() -> Self {
        Self {
            diags: vec![],
        }
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
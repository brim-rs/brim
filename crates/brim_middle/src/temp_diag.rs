use crate::lints::Lint;
use brim_diagnostics::{
    ErrorEmitted,
    diagnostic::{Diagnostic, ToDiagnostic},
};
use std::fmt::Debug;

#[derive(Clone)]
/// Struct to store diagnostics to be later emitted by the main diagnostic context
pub struct TemporaryDiagnosticContext {
    pub diags: Vec<Diagnostic<usize>>,
}

impl Default for TemporaryDiagnosticContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TemporaryDiagnosticContext {
    pub fn new() -> Self {
        Self { diags: vec![] }
    }

    pub fn emit(&mut self, diag: Box<dyn ToDiagnostic>) {
        self.diags.push(diag.to_diagnostic());
    }

    pub fn emit_diag(&mut self, diag: Diagnostic<usize>) {
        self.diags.push(diag);
    }

    pub fn emit_impl(&mut self, diag: impl ToDiagnostic) -> ErrorEmitted {
        self.diags.push(diag.to_diagnostic());

        ErrorEmitted::new()
    }

    pub fn extend(&mut self, diags: Vec<Diagnostic<usize>>) {
        self.diags.extend(diags);
    }

    pub fn emit_lint(&mut self, ling: Lint) {
        if ling.enabled {
            self.emit(ling.diag);
        }
    }
}

impl Debug for TemporaryDiagnosticContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TemporaryDiagnosticContext").field("diags", &self.diags.len()).finish()
    }
}

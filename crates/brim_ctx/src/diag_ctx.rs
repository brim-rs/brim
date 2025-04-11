use brim_diagnostics::{
    diagnostic::{Diagnostic, ToDiagnostic},
    term::{DiagConfig, emit},
};
use brim_middle::temp_diag::TemporaryDiagnosticContext;
use brim_span::files::SimpleFiles;
use std::{fmt::Debug, io::stderr};

#[derive(Debug, Clone)]
pub struct DiagnosticContext {
    pub config: DiagConfig,
}

impl Default for DiagnosticContext {
    fn default() -> Self {
        Self::new()
    }
}

impl DiagnosticContext {
    pub fn new() -> Self {
        Self { config: DiagConfig::default() }
    }

    pub fn emit(&mut self, diag: &dyn ToDiagnostic, files: &SimpleFiles) {
        let diag = diag.to_diagnostic();

        self.emit_inner(&diag, files);
    }

    pub fn emit_inner(&mut self, diag: &Diagnostic<usize>, files: &SimpleFiles) {
        emit(&mut stderr(), &self.config, files, &diag).unwrap();
    }

    pub fn extend_temporary(&mut self, temp: &TemporaryDiagnosticContext, files: &SimpleFiles) {
        for diag in &temp.diags {
            self.emit_inner(&diag.clone(), files);
        }
    }
}

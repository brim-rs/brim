use std::fmt::Debug;
use brim_diagnostics::{
    diagnostic::ToDiagnostic,
    term::{DiagConfig, emit},
};
use brim_span::files::SimpleFiles;
use std::io::stderr;

#[derive(Debug)]
pub struct DiagnosticContext {
    pub config: DiagConfig,
}

impl DiagnosticContext {
    pub fn new() -> Self {
        Self {
            config: DiagConfig::default(),
        }
    }

    pub fn emit<'diags>(&mut self, diag: Box<dyn ToDiagnostic<'diags> + 'diags>, files: &SimpleFiles) {
        let diag = diag.to_diagnostic();
        emit(&mut stderr(), &self.config, files, &diag).unwrap();
    }

    pub fn from_temporary<'diags>(&mut self, temp: TemporaryDiagnosticContext<'diags>, files: &SimpleFiles) {
        for diag in temp.diags {
            self.emit(diag, files);
        }
    }
}

/// Struct to store diagnostics to be later emitted by the main diagnostic context
pub struct TemporaryDiagnosticContext<'diags> {
    pub diags: Vec<Box<dyn ToDiagnostic<'diags> + 'diags>>,
}

impl<'diags> TemporaryDiagnosticContext<'diags> {
    pub fn new() -> Self {
        Self {
            diags: vec![],
        }
    }

    pub fn emit(&mut self, diag: Box<dyn ToDiagnostic<'diags> + 'diags>) {
        self.diags.push(diag);
    }
}

impl Debug for TemporaryDiagnosticContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TemporaryDiagnosticContext")
            .field("diags", &self.diags.len())
            .finish()
    }
}
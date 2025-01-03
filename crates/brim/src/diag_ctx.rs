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

    pub fn emit<'diags>(&mut self, diag: impl ToDiagnostic<'diags>, files: &SimpleFiles) {
        let diag = diag.to_diagnostic();

        emit(&mut stderr(), &self.config, files, &diag).unwrap();
    }
}

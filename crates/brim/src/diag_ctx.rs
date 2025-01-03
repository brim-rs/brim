use std::io::stderr;
use brim_diagnostics::diagnostic::{Diagnostic, ToDiagnostic};
use brim_diagnostics::term::{emit, DiagConfig};
use brim_shell::Shell;
use brim_span::file::FileId;
use brim_span::files::SimpleFiles;

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

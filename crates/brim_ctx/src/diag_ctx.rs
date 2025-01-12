use std::fmt::Debug;
use brim_diagnostics::{diagnostic::ToDiagnostic, term::{DiagConfig, emit}, TemporaryDiagnosticContext};
use brim_span::files::SimpleFiles;
use std::io::stderr;
use brim_diagnostics::diagnostic::Diagnostic;

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

    pub fn emit<'diags>(&mut self, diag: &Box<dyn ToDiagnostic<'diags> + 'diags>, files: &SimpleFiles) {
        let diag = diag.to_diagnostic();

        self.emit_inner(diag, files);
    }

    pub fn emit_inner<'diags>(&mut self, diag: Diagnostic<'diags, usize>, files: &SimpleFiles) {
        emit(&mut stderr(), &self.config, files, &diag).unwrap();
    }

    pub fn from_temporary<'diags>(&mut self, temp: &TemporaryDiagnosticContext<'diags>, files: &SimpleFiles) {
        for diag in temp.diags.iter() {
            self.emit_inner(diag.clone(), files);
        }
    }
}

use brim_ast::ErrorEmitted;
use brim_diagnostics::diagnostic::ToDiagnostic;
use brim_span::files::{files, SimpleFiles};
use crate::diag_ctx::DiagnosticContext;

#[derive(Debug)]
pub struct CompilerContext {
    dcx: DiagnosticContext,
}

impl CompilerContext {
    pub fn new() -> Self {
        Self {
            dcx: DiagnosticContext::new(),
        }
    }

    pub fn dcx(&mut self) -> &mut DiagnosticContext {
        &mut self.dcx
    }

    pub fn emit<'a>(&mut self, diag: impl ToDiagnostic<'a>) -> ErrorEmitted {
        self.dcx.emit(diag, &SimpleFiles::from_files(files()));
        
        ErrorEmitted::new()
    }
}

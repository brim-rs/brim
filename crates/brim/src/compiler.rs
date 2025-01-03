use brim_ast::ErrorEmitted;
use brim_diagnostics::diagnostic::ToDiagnostic;
use brim_span::files::SimpleFiles;
use crate::diag_ctx::DiagnosticContext;

#[derive(Debug)]
pub struct CompilerContext<'a> {
    dcx: DiagnosticContext,
    files: &'a mut SimpleFiles,
}

impl<'a> CompilerContext<'a> {
    pub fn new(files: &'a mut SimpleFiles) -> Self {
        Self {
            dcx: DiagnosticContext::new(),
            files,
        }
    }

    pub fn dcx(&mut self) -> &mut DiagnosticContext {
        &mut self.dcx
    }

    pub fn emit(&mut self, diag: impl ToDiagnostic<'a>) -> ErrorEmitted {
        self.dcx.emit(diag, &self.files);

        ErrorEmitted::new()
    }
}

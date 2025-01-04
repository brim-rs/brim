use std::marker::PhantomData;
use brim_ast::ErrorEmitted;
use brim_diagnostics::diagnostic::{Diagnostic, ToDiagnostic};
use brim_span::files::{files, SimpleFiles};
use crate::diag_ctx::DiagnosticContext;

#[derive(Debug)]
pub struct CompilerContext<'a> {
    dcx: DiagnosticContext,
    #[cfg(feature = "snap")]
    pub emitted: Vec<Diagnostic<'a, usize>>,
    /// Required while snap is not enabled
    marker: PhantomData<&'a ()>
}

impl<'a> CompilerContext<'a> {
    pub fn new() -> Self {
        Self {
            dcx: DiagnosticContext::new(),
            #[cfg(feature = "snap")]
            emitted: vec![],
            marker: PhantomData,
        }
    }

    pub fn dcx(&mut self) -> &mut DiagnosticContext {
        &mut self.dcx
    }

    pub fn emit(&mut self, diag: impl ToDiagnostic<'a>) -> ErrorEmitted {
        #[cfg(not(feature = "snap"))]
        self.dcx.emit(diag, &SimpleFiles::from_files(files()));
        
        #[cfg(feature = "snap")]
        self.emitted.push(diag.to_diagnostic());
        
        ErrorEmitted::new()
    }
}

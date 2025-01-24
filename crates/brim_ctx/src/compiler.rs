use crate::diag_ctx::DiagnosticContext;
use brim_ast::ErrorEmitted;
use brim_diagnostics::diagnostic::{Diagnostic, ToDiagnostic};
#[cfg(not(feature = "snap"))]
use brim_span::files::{SimpleFiles, files};
use std::marker::PhantomData;

#[derive(Debug)]
pub struct CompilerContext<'a> {
    dcx: DiagnosticContext,
    #[cfg(feature = "snap")]
    pub emitted: Vec<Diagnostic<'a, usize>>,
    marker: PhantomData<&'a ()>,
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

    pub fn emit(&mut self, diag: impl ToDiagnostic<'a> + 'a) -> ErrorEmitted {
        self.emit_inner(Box::new(diag))
    }

    pub fn emit_diag(&mut self, diag: Diagnostic<'a, usize>) -> ErrorEmitted {
        #[cfg(not(feature = "snap"))]
        self.dcx.emit_inner(diag, &SimpleFiles::from_files(files()));

        #[cfg(feature = "snap")]
        self.emitted.push(diag);

        ErrorEmitted::new()
    }

    pub fn emit_inner(&mut self, diag: Box<dyn ToDiagnostic<'a> + 'a>) -> ErrorEmitted {
        #[cfg(not(feature = "snap"))]
        self.dcx
            .emit(&Box::new(diag), &SimpleFiles::from_files(files()));

        #[cfg(feature = "snap")]
        self.emitted.push(diag.to_diagnostic());

        ErrorEmitted::new()
    }
}

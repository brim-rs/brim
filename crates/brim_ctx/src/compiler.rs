use crate::diag_ctx::DiagnosticContext;
use brim_ast::ErrorEmitted;
use brim_diagnostics::diagnostic::{Diagnostic, ToDiagnostic};
#[cfg(not(feature = "snap"))]
use brim_span::files::{SimpleFiles, files};
use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub struct CompilerContext<'a> {
    dcx: DiagnosticContext,
    pub emitted: Vec<Diagnostic<'a, usize>>,
}

impl<'a> CompilerContext<'a> {
    pub fn new() -> Self {
        Self {
            dcx: DiagnosticContext::new(),
            emitted: vec![],
        }
    }

    pub fn dcx(&mut self) -> &mut DiagnosticContext {
        &mut self.dcx
    }

    pub fn emit(&mut self, diag: impl ToDiagnostic<'a> + 'a) -> ErrorEmitted {
        self.emit_inner(Box::new(diag))
    }

    pub fn emit_diag(&mut self, diag: Diagnostic<'a, usize>) -> ErrorEmitted {
        self.dcx.emit_inner(diag.clone(), &SimpleFiles::from_files(files()));
        self.emitted.push(diag);

        ErrorEmitted::new()
    }

    pub fn emit_inner(&mut self, diag: Box<dyn ToDiagnostic<'a> + 'a>) -> ErrorEmitted {
        let diag_clone = diag.to_diagnostic();
        self.dcx
            .emit(&Box::new(diag), &SimpleFiles::from_files(files()));
        self.emitted.push(diag_clone);

        ErrorEmitted::new()
    }
}

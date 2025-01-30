pub mod reporting;

use crate::diagnostic::{Diagnostic, ToDiagnostic};
pub use reporting::*;
use std::fmt::Debug;

#[macro_export]
macro_rules! box_diag {
    ($diag:expr) => {
        return Err(Box::new($diag))
    };
    (@opt $diag:expr) => {
        return Some(Box::new($diag))
    };
}

#[macro_export]
macro_rules! diag_opt {
    ($ctx:expr, $diag:expr) => {
        if let Some(diag) = $diag {
            $ctx.emit(diag);
        }
    };
}

pub type OptionalDiag<'a> = Option<Box<dyn ToDiagnostic>>;

/// Struct to store diagnostics to be later emitted by the main diagnostic context
pub struct TemporaryDiagnosticContext {
    pub diags: Vec<Diagnostic<usize>>,
}

/// A struct that represents already emitted diagnostic
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ErrorEmitted(());

impl ErrorEmitted {
    pub fn new() -> Self {
        Self(())
    }
}

impl PartialEq<()> for ErrorEmitted {
    fn eq(&self, _: &()) -> bool {
        true
    }
}

impl TemporaryDiagnosticContext {
    pub fn new() -> Self {
        Self { diags: vec![] }
    }

    pub fn emit(&mut self, diag: Box<dyn ToDiagnostic>) {
        self.diags.push(diag.to_diagnostic());
    }

    pub fn emit_impl(&mut self, diag: impl ToDiagnostic) -> ErrorEmitted {
        self.diags.push(diag.to_diagnostic());

        ErrorEmitted::new()
    }

    pub fn extend(&mut self, diags: Vec<Diagnostic<usize>>) {
        self.diags.extend(diags);
    }
}

impl Debug for TemporaryDiagnosticContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TemporaryDiagnosticContext")
            .field("diags", &self.diags.len())
            .finish()
    }
}

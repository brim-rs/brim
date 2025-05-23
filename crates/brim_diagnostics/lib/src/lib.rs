pub mod reporting;

use crate::diagnostic::ToDiagnostic;
pub use reporting::*;
use std::fmt::Debug;

#[macro_export]
macro_rules! diag_opt {
    ($ctx:expr, $diag:expr) => {
        if let Some(diag) = $diag {
            $ctx.emit(diag);
        }
    };
}

pub type OptionalDiag<'a> = Option<Box<dyn ToDiagnostic>>;

/// A struct that represents already emitted diagnostic
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ErrorEmitted(());

impl Default for ErrorEmitted {
    fn default() -> Self {
        Self::new()
    }
}

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

//! Terminal back-end for emitting diagnostics.

use brim_span::files::Files;
use std::io::Write;

mod config;
mod renderer;
mod views;

pub use self::config::{Chars, DiagConfig, Styles};
use crate::reporting::diagnostic::Diagnostic;

pub fn emit<'files, F: Files<'files>>(
    writer: &mut dyn Write,
    config: &DiagConfig,
    files: &'files F,
    diagnostic: &Diagnostic<F::FileId>,
) -> Result<(), brim_span::files::FilesError> {
    use self::{renderer::Renderer, views::RichDiagnostic};

    let mut renderer = Renderer::new(writer, config);
    RichDiagnostic::new(diagnostic, config).render(files, &mut renderer)
}

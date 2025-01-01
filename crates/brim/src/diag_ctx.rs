use crate::diagnostics::Diagnostic;

#[derive(Debug)]
pub struct DiagnosticContext<'diags> {
    has_printed: bool,
    stored: Vec<Diagnostic<'diags>>,
}

impl<'diags> DiagnosticContext<'diags> {
    pub fn new() -> Self { DiagnosticContext { has_printed: false, stored: vec![] } }
}
use brim_diagnostics::diagnostic::Label;

#[derive(Debug)]
/// This is a wrapper around [`Diagnostic`](brim_diagnostics::diagnostic::Diagnostic).
pub struct Diagnostic<'label> {
    pub message: String,
    pub code: String,
    pub labels: Vec<Label<'label, usize>>,
}

impl Diagnostic<'_> {
    pub fn new(message: impl ToString, code: impl ToString) -> Diagnostic<'static> {
        Diagnostic {
            message: message.to_string(),
            code: code.to_string(),
            labels: Vec::new(),
        }
    }
}

pub trait IntoDiagnostic {
    fn into_diagnostic(self) -> Diagnostic<'static>;
}

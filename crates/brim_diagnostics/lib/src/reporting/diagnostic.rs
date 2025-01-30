use std::{ops::Range, string::ToString};

#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Help,
    Note,
    Warning,
    Error,
    Bug,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub enum LabelStyle {
    Primary,
    Warning,
    Error,
    Add(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Label<FileId> {
    pub style: LabelStyle,
    pub file_id: FileId,
    pub range: Range<usize>,
    pub message: String,
}

impl<'a, FileId> Label<FileId> {
    pub fn new(
        style: LabelStyle,
        file_id: FileId,
        range: impl Into<Range<usize>>,
    ) -> Label<FileId> {
        Label {
            style,
            file_id,
            range: range.into(),
            message: String::new(),
        }
    }

    pub fn primary(file_id: FileId, range: impl Into<Range<usize>>) -> Label<FileId> {
        Label::new(LabelStyle::Primary, file_id, range)
    }

    pub fn error(file_id: FileId, range: impl Into<Range<usize>>) -> Label<FileId> {
        Label::new(LabelStyle::Error, file_id, range)
    }

    pub fn warning(file_id: FileId, range: impl Into<Range<usize>>) -> Label<FileId> {
        Label::new(LabelStyle::Warning, file_id, range)
    }

    pub fn add(
        file_id: FileId,
        to_add: impl Into<String>,
        range: impl Into<Range<usize>>,
    ) -> Label<FileId> {
        Label::new(LabelStyle::Add(to_add.into()), file_id, range)
    }

    pub fn with_message(mut self, message: impl ToString) -> Label<FileId> {
        self.message = message.to_string();
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic<FileId> {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<Label<FileId>>,
    pub notes: Vec<String>,
}

impl<'a, FileId> Diagnostic<FileId> {
    pub fn new(severity: Severity) -> Diagnostic<FileId> {
        Diagnostic {
            severity,
            code: None,
            message: String::new(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn bug() -> Diagnostic<FileId> {
        Diagnostic::new(Severity::Bug)
    }

    pub fn error() -> Diagnostic<FileId> {
        Diagnostic::new(Severity::Error)
    }

    pub fn warning() -> Diagnostic<FileId> {
        Diagnostic::new(Severity::Warning)
    }

    pub fn help() -> Diagnostic<FileId> {
        Diagnostic::new(Severity::Help)
    }

    pub fn with_code(mut self, code: impl ToString) -> Diagnostic<FileId> {
        self.code = Some(code.to_string());
        self
    }

    pub fn with_message(mut self, message: impl ToString) -> Diagnostic<FileId> {
        self.message = message.to_string();
        self
    }

    pub fn with_labels(mut self, mut labels: Vec<Label<FileId>>) -> Diagnostic<FileId> {
        self.labels.append(&mut labels);
        self
    }

    pub fn with_notes(mut self, mut notes: Vec<String>) -> Diagnostic<FileId> {
        self.notes.append(&mut notes);
        self
    }
}

pub trait ToDiagnostic {
    fn message(&self) -> String;
    fn code(&self) -> Option<String>;
    fn severity(&self) -> Severity;
    fn labels(&self) -> Vec<Label<usize>>;
    fn notes(&self) -> Vec<String>;

    fn to_diagnostic(&self) -> Diagnostic<usize> {
        Diagnostic::new(self.severity())
            .with_code(self.code().unwrap_or_default())
            .with_message(self.message())
            .with_labels(self.labels())
            .with_notes(self.notes())
    }
}

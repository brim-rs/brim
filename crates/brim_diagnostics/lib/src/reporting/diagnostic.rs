use std::{ops::Range, string::ToString};

#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Help,
    Note,
    Warning,
    Error,
    Bug,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd)]
pub enum LabelStyle<'a> {
    Primary,
    Warning,
    Error,
    Add(&'a str),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Label<'a, FileId> {
    pub style: LabelStyle<'a>,
    pub file_id: FileId,
    pub range: Range<usize>,
    pub message: String,
}

impl<'a, FileId> Label<'a, FileId> {
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

    pub fn primary(file_id: FileId, range: impl Into<Range<usize>>) -> Label<'a, FileId> {
        Label::new(LabelStyle::Primary, file_id, range)
    }

    pub fn error(file_id: FileId, range: impl Into<Range<usize>>) -> Label<'a, FileId> {
        Label::new(LabelStyle::Error, file_id, range)
    }

    pub fn warning(file_id: FileId, range: impl Into<Range<usize>>) -> Label<'a, FileId> {
        Label::new(LabelStyle::Warning, file_id, range)
    }

    pub fn add(file_id: FileId, to_add: impl Into<&'a str>, range: impl Into<Range<usize>>) -> Label<'a, FileId> {
        Label::new(LabelStyle::Add(to_add.into()), file_id, range)
    }

    pub fn with_message(mut self, message: impl ToString) -> Label<'a, FileId> {
        self.message = message.to_string();
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic<'a, FileId> {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<Label<'a, FileId>>,
    pub notes: Vec<String>,
}

impl<'a,FileId> Diagnostic<'a,FileId> {
    pub fn new(severity: Severity) -> Diagnostic<'a,FileId> {
        Diagnostic {
            severity,
            code: None,
            message: String::new(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn bug() -> Diagnostic<'a,FileId> {
        Diagnostic::new(Severity::Bug)
    }

    pub fn error() -> Diagnostic<'a,FileId> {
        Diagnostic::new(Severity::Error)
    }

    pub fn warning() -> Diagnostic<'a,FileId> {
        Diagnostic::new(Severity::Warning)
    }

    pub fn note() -> Diagnostic<'a,FileId> {
        Diagnostic::new(Severity::Note)
    }

    pub fn help() -> Diagnostic<'a,FileId> {
        Diagnostic::new(Severity::Help)
    }

    pub fn with_code(mut self, code: impl ToString) -> Diagnostic<'a,FileId> {
        self.code = Some(code.to_string());
        self
    }

    pub fn with_message(mut self, message: impl ToString) -> Diagnostic<'a,FileId> {
        self.message = message.to_string();
        self
    }

    pub fn with_labels(mut self, mut labels: Vec<Label<'a, FileId>>) -> Diagnostic<'a,FileId> {
        self.labels.append(&mut labels);
        self
    }

    pub fn with_notes(mut self, mut notes: Vec<String>) -> Diagnostic<'a,FileId> {
        self.notes.append(&mut notes);
        self
    }
}

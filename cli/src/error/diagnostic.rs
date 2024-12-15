use std::collections::HashMap;
use std::io::{stderr, Write};
use std::sync::Arc;
use brim_shell::Shell;
use brim_shell::styles::{ERROR, WARN};
use crate::error::span::TextSpan;
use crate::lexer::source::Source;
use anyhow::Result;
use codespan_reporting::diagnostic::{Diagnostic as Diag, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::{emit, Config};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub text: String,
    pub level: Level,
    pub labels: Vec<(TextSpan, Option<String>)>,
    pub hint: Vec<String>,
    pub code: Option<String>,
}

impl Diagnostic {
    pub fn write(&self, source: Option<Arc<Source>>) -> Result<()> {
        if let Some(ref source) = source {
            let mut files = SimpleFiles::new();

            let file_id = files.add(
                source.path.clone().to_string_lossy().to_string(),
                source.content.to_string(),
            );

            let labels = self.labels.iter().map(|(span, message)| {
                let label = Label::primary(file_id, span.start.index..span.end.index);

                if let Some(message) = message {
                    label.with_message(message.clone())
                } else {
                    label
                }
            }).collect();

            let diagnostic = if self.level == Level::Warning {
                Diag::warning()
            } else {
                Diag::error()
            }
                .with_message(self.text.clone())
                .with_labels(labels)
                .with_notes(
                    self.hint.iter().map(|hint| unindent::unindent(&hint.clone())).collect(),
                );

            let diagnostic = if let Some(code) = &self.code {
                diagnostic.with_code(code.clone())
            } else {
                diagnostic
            };

            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = Config::default();

            emit(&mut writer.lock(), &config, &files, &diagnostic)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Diagnostics {
    pub diagnostics: Vec<(Diagnostic, Option<Arc<Source>>)>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    pub fn warning(&mut self, message: String, source: Option<Arc<Source>>, labels: Vec<(TextSpan, Option<String>)>, hint: Vec<String>) {
        self.diagnostics.push((Diagnostic {
            text: message,
            level: Level::Warning,
            labels,
            hint,
            code: None,
        }, source));
    }

    pub fn new_diagnostic(&mut self, diagnostic: Diagnostic, source: Arc<Source>) {
        self.diagnostics.push((diagnostic, Some(source)));
    }

    pub fn print_diagnostics(&mut self) {
        for (diagnostic, source) in self.diagnostics.clone() {
            diagnostic.write(source).unwrap();
        }
    }
}
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
    pub fn write(&self, shell: &mut Shell, source: Option<Arc<Source>>) -> Result<()> {
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
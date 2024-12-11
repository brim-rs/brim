use std::io::{stderr, Write};
use std::sync::Arc;
use brim_shell::Shell;
use brim_shell::styles::{ERROR, WARN};
use crate::error::span::TextSpan;
use crate::lexer::source::Source;
use anyhow::Result;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub text: String,
    pub level: Level,
    pub span: Option<TextSpan>,
    pub hint: Option<String>,
}

impl Diagnostic {
    pub fn write(&self, shell: &mut Shell, source: Option<Arc<Source>>) -> Result<()> {
        let mut buf = stderr();
        let level_str = match self.level {
            Level::Error => "error",
            Level::Warning => "warning",
        }.to_string();
        let style = match self.level {
            Level::Error => ERROR,
            Level::Warning => WARN,
        };
        write!(buf, "{}", shell.write(&level_str, Some(&self.text), &style, false)?)?;

        Ok(())
    }
}
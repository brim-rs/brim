pub mod diagnostic;
pub mod position;
pub mod span;

use crate::error::{
    diagnostic::{Diagnostic, Level},
    span::TextSpan,
};
use std::io;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BrimError {
    #[error("CLI error")]
    CliError { message: String },

    #[error("Lexer error")]
    LexerError {
        message: String,
        labels: Vec<(TextSpan, Option<String>)>,
        hint: Vec<String>,
        code: Option<String>,
    },

    #[error("Parser error")]
    ParserError {
        message: String,
        labels: Vec<(TextSpan, Option<String>)>,
        hint: Vec<String>,
        code: Option<String>,
    },

    #[error("Other error")]
    OtherError { source: anyhow::Error },
}

pub fn lexer_error(
    message: String,
    labels: Vec<(TextSpan, Option<String>)>,
    hint: Vec<String>,
    code: Option<String>,
) -> BrimError {
    BrimError::LexerError {
        message,
        labels,
        hint,
        code,
    }
}

pub fn parser_error(
    message: String,
    labels: Vec<(TextSpan, Option<String>)>,
    hint: Vec<String>,
    code: Option<String>,
) -> BrimError {
    BrimError::ParserError {
        message,
        labels,
        hint,
        code,
    }
}

pub fn expected_token(
    expected: String,
    hint: Vec<String>,
    labels: Vec<(TextSpan, Option<String>)>,
) -> BrimError {
    BrimError::ParserError {
        message: format!("Expected token: {}", expected),
        labels,
        hint,
        code: None,
    }
}

pub fn invalid_token(token: String, labels: Vec<(TextSpan, Option<String>)>) -> BrimError {
    BrimError::LexerError {
        message: format!("Found invalid token: {}", token),
        labels,
        hint: vec![],
        code: None,
    }
}

impl From<io::Error> for BrimError {
    fn from(error: io::Error) -> Self {
        BrimError::OtherError {
            source: error.into(),
        }
    }
}

impl BrimError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            BrimError::CliError { message } => Diagnostic {
                text: message.clone(),
                level: Level::Error,
                hint: vec![
                    "Please check the CLI usage with `brim --help` and try again".to_string(),
                ],
                code: None,
                labels: vec![],
            },
            BrimError::OtherError { source } => Diagnostic {
                text: source.to_string(),
                level: Level::Error,
                hint: vec![],
                code: None,
                labels: vec![],
            },
            BrimError::LexerError {
                message,
                labels,
                hint,
                code,
            }
            | BrimError::ParserError {
                message,
                labels,
                hint,
                code,
            } => Diagnostic {
                text: message.clone(),
                level: Level::Error,
                labels: labels.clone(),
                hint: hint.clone(),
                code: code.clone(),
            },
        }
    }
}

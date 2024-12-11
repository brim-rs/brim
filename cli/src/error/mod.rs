pub mod diagnostic;
pub mod span;
pub mod position;

use std::io;
use thiserror::Error;
use crate::error::diagnostic::{Diagnostic, Level};
use crate::error::span::TextSpan;

#[derive(Error, Debug)]
pub enum BrimError {
    #[error("CLI error")]
    CliError {
        message: String,
    },

    #[error("Lexer error")]
    LexerError {
        message: String,
        span: TextSpan,
        hint: Option<String>,
    },

    #[error("Other error")]
    OtherError {
        source: anyhow::Error
    },
}

pub fn lexer_error(message: String, span: TextSpan, hint: Option<String>) -> BrimError {
    BrimError::LexerError {
        message,
        span,
        hint,
    }
}

pub fn invalid_token(
    token: String, span: TextSpan
) -> BrimError {
    BrimError::LexerError {
        message: format!("Found invalid token: {}", token),
        span,
        hint: None,
    }
}

impl From<io::Error> for BrimError {
    fn from(error: io::Error) -> Self {
        BrimError::OtherError {
            source: error.into()
        }
    }
}

impl BrimError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            BrimError::CliError { message } => Diagnostic {
                text: message.clone(),
                level: Level::Error,
                span: None,
                hint: Some("Please check the CLI usage with `brim --help` and try again".to_string()),
            },
            BrimError::OtherError { source } => Diagnostic {
                text: source.to_string(),
                level: Level::Error,
                span: None,
                hint: None,
            },
            BrimError::LexerError { message, span, hint } => Diagnostic {
                text: message.clone(),
                level: Level::Error,
                span: Some(span.clone()),
                hint: hint.clone(),
            },
        }
    }
}
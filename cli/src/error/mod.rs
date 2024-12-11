pub mod diagnostic;
pub mod span;
pub mod position;

use std::io;
use thiserror::Error;
use crate::error::diagnostic::{Diagnostic, Level};

#[derive(Error, Debug)]
pub enum BrimError {
    #[error("CLI error")]
    CliError {
        message: String,
    },

    #[error("Other error")]
    OtherError {
        source: anyhow::Error
    },
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
                source: None,
                span: None,
                hint: Some("Please check the CLI usage with `brim --help` and try again".to_string()),
            },
            BrimError::OtherError { source } => Diagnostic {
                text: source.to_string(),
                level: Level::Error,
                source: None,
                span: None,
                hint: None,
            },
        }
    }
}
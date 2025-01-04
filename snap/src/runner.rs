use anyhow::{Result, bail};
use brim::{
    compiler::CompilerContext,
    files::{SimpleFile, SimpleFiles, files},
    span::Span,
    symbol::{Symbol, SymbolInterner},
    token::TokenKind,
};
use brim_parser::{parser::Parser, parser_from_simple_file};
use brim_shell::Shell;
use once_cell::sync::Lazy;
use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};
use std::time::Instant;
use crate::reporter::report_results;

pub static TEST_RESULTS: Lazy<Arc<Mutex<TestSuite>>> =
    Lazy::new(|| Arc::new(Mutex::new(TestSuite::new())));

#[derive(Debug, Clone)]
pub struct Comment {
    pub directive: Directive,
    pub comment: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Directive {
    ExpectError,
    Unknown(String),
}

impl Directive {
    fn from_str(s: &str) -> Self {
        match s {
            "expect_error" => Self::ExpectError,
            unknown => Self::Unknown(unknown.to_string()),
        }
    }
}

#[derive(Debug)]
pub struct TestResult {
    pub directive: Directive,
    pub comment: String,
    pub result: Result<(), TestFail>,
    pub location: TestLocation,
}

#[derive(Debug)]
pub struct TestLocation {
    pub file: String,
}

#[derive(Debug)]
pub struct TestFail {
    pub reason: String,
}

#[derive(Debug, Default)]
pub struct TestSuite {
    pub results: Vec<TestResult>,
    pub total_tests: usize,
    pub passed_tests: usize,
}

impl TestSuite {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_result(&mut self, result: TestResult) {
        self.total_tests += 1;
        if result.result.is_ok() {
            self.passed_tests += 1;
        }
        self.results.push(result);
    }
}

pub fn run_tests(shell: &mut Shell, start: Instant) -> Result<()> {
    let mut overall_success = true;

    for file in files() {
        if let Err(e) = run_file_tests(&file, shell) {
            eprintln!("Error testing file {:?}: {}", file, e);
            overall_success = false;
        }
    }

    report_results(start)?;
    Ok(())
}

fn run_file_tests(file: &SimpleFile, shell: &mut Shell) -> Result<()> {
    let ctx = &mut CompilerContext::new();
    let mut parser = parser_from_simple_file(file)?;
    parser.keep_comments = true;
    parser.parse_barrel(ctx)?;

    let comments = directive_comments(&parser);

    for comment in comments {
        run_comment_directive(&comment, ctx, shell, file)?;
    }

    Ok(())
}

pub fn directive_comments(parser: &Parser) -> Vec<Comment> {
    parser
        .tokens
        .iter()
        .filter_map(|token| {
            if let TokenKind::DocComment(_) = token.kind {
                let comment = token.as_comment();
                let string = comment.as_str()?;
                let string = string.trim();

                if !string.starts_with('@') {
                    return None;
                }

                // Remove the '@' prefix
                let string = &string[1..];
                let parts: Vec<&str> = string.splitn(2, ':').map(str::trim).collect();

                let directive = parts.get(0).map(|&s| Directive::from_str(s))?;
                let comment = parts.get(1).map_or("", |&c| c).to_string();

                Some(Comment {
                    directive,
                    comment,
                    span: token.span,
                })
            } else {
                None
            }
        })
        .collect()
}

pub fn run_comment_directive(
    comment: &Comment,
    comp: &mut CompilerContext,
    shell: &mut Shell,
    file: &SimpleFile,
) -> Result<()> {
    let result = match &comment.directive {
        Directive::ExpectError => {
            let expected_message = &comment.comment;
            let diagnostics = &comp.emitted;

            if diagnostics.is_empty() {
                return Err(anyhow::anyhow!("Expected error message but got none"));
            }

            let mut found_message = false;
            for diagnostic in diagnostics {
                if diagnostic.message.contains(expected_message) {
                    found_message = true;
                    break;
                }
            }

            if !found_message {
                bail!(
                    "Directive 'expect_error' failed: expected error message '{}' but got none",
                    expected_message
                );
            }

            Ok(())
        }
        Directive::Unknown(directive) => Err(anyhow::anyhow!("Unknown directive: {}", directive)),
    };

    let test_result = TestResult {
        directive: comment.directive.clone(),
        comment: comment.comment.clone(),
        result: result.map_err(|e| TestFail {
            reason: e.to_string(),
        }),
        location: TestLocation {
            file: file.name().to_string_lossy().to_string(),
        },
    };

    TEST_RESULTS.lock().unwrap().add_result(test_result);
    Ok(())
}

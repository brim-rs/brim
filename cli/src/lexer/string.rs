use std::fs;
use anyhow::{bail, Result};
use crate::error::lexer_error;
use crate::error::span::TextSpan;
use crate::lexer::Lexer;
use crate::lexer::tokens::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {}

impl StringLiteral {
    pub fn lex_string(lexer: &mut Lexer) -> Result<TokenKind> {
        let string = StringLiteral::consume_string(lexer)?;
        Ok(TokenKind::String(string))
    }

    pub fn consume_string(lexer: &mut Lexer) -> Result<String> {
        let mut str = String::new();

        lexer.consume();

        while let Some(c) = lexer.current() {
            if c == '"' {
                lexer.consume();
                break;
            }

            if c == '\\' {
                lexer.consume();
                if let Some(next) = lexer.current() {
                    match next {
                        'n' => str.push('\n'),
                        'r' => str.push('\r'),
                        't' => str.push('\t'),
                        '\\' => str.push('\\'),
                        '"' => str.push('"'),
                        _ => {
                            bail!(lexer_error(
                                format!("Invalid escape sequence: {}", next.to_string()),
                                vec![((TextSpan::new(lexer.position, lexer.position, next.to_string()), 
                                    format!("{} is invalid escape sequence", next.to_string()).into()
                                ))],
                                vec![],
                                None
                            ))
                        }
                    }
                    lexer.consume();
                }
            } else {
                str.push(c);
                lexer.consume();
            }
        }

        Ok(str)
    }
}
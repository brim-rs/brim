use crate::lexer::{tokens::TokenKind, Lexer};
use anyhow::Result;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {}

impl Identifier {
    pub fn is_identifier_start(c: char) -> bool {
        matches!(c as u32, 0x0024 /* $ */ | 0x005F /* _ */) || c.is_alphabetic()
    }
}

impl Identifier {
    pub fn lex_identifier(lexer: &mut Lexer) -> Result<TokenKind> {
        let ident = Identifier::consume_identifier(lexer);
        Ok(match ident.as_str() {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "null" => TokenKind::Null,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "use" => TokenKind::Use,
            "pub" => TokenKind::Pub,
            "from" => TokenKind::From,
            "catch" => TokenKind::Catch,
            "loop" => TokenKind::Loop,
            "struct" => TokenKind::Struct,
            "impl" => TokenKind::Impl,
            "trait" => TokenKind::Trait,
            "then" => TokenKind::Then,
            "const" => TokenKind::Const,
            "enum" => TokenKind::Enum,

            _ => TokenKind::Identifier,
        })
    }

    /// Consume an identifier.
    pub fn consume_identifier(lexer: &mut Lexer) -> String {
        let mut ident = String::new();

        while let Some(c) = lexer.current() {
            if Identifier::is_identifier_start(c) || c.is_digit(10) {
                ident.push(c);
            } else {
                break;
            }
            lexer.consume();
        }

        ident
    }
}

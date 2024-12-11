use crate::error::invalid_token;
use crate::error::span::TextSpan;
use crate::lexer::Lexer;
use crate::lexer::tokens::TokenKind;
use anyhow::Result;

#[derive(Debug, Clone, PartialEq)]
pub struct Punctuation;

impl Punctuation {
    pub fn lex_punctuation(lexer: &mut Lexer, c: char) -> Result<TokenKind> {
        let kind = match c {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            ',' => TokenKind::Comma,
            '.' => lexer.lex_potential_triple(
                '.',
                TokenKind::Dot,
                TokenKind::DoubleDot,
                TokenKind::TripleDot,
            ),
            ':' => lexer.lex_potential_double(':', TokenKind::Colon, TokenKind::DoubleColon),
            ';' => TokenKind::Semicolon,
            '/' => {
                if lexer.match_next('/') {
                    while let Some(c) = lexer.current() {
                        if c == '\n' {
                            break;
                        }
                        lexer.consume();
                    }
                    TokenKind::Comment
                } else {
                    lexer.lex_potential_double(
                        '=',
                        TokenKind::Slash,
                        TokenKind::DivideEquals,
                    )
                }
            }
            '+' => {
                if lexer.match_next('+') {
                    lexer.consume();
                    TokenKind::Increment
                } else if lexer.match_next('=') {
                    lexer.consume();
                    TokenKind::PlusEquals
                } else {
                    TokenKind::Plus
                }
            }
            '-' => {
                if lexer.match_next('-') {
                    lexer.consume();
                    TokenKind::Decrement
                } else if lexer.match_next('=') {
                    lexer.consume();
                    TokenKind::MinusEquals
                } else if lexer.match_next('>') {
                    lexer.consume();
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '*' => {
                if lexer.match_next('*') {
                    lexer.consume();
                    TokenKind::DoubleAsterisk
                } else if lexer.match_next('=') {
                    lexer.consume();
                    TokenKind::MultiplyEquals
                } else {
                    TokenKind::Asterisk
                }
            }
            '%' => TokenKind::Percent,
            '^' => TokenKind::Caret,
            '!' => lexer.lex_potential_double('=', TokenKind::Bang, TokenKind::BangEquals),
            '=' => {
                lexer.lex_potential_double('=', TokenKind::Equals, TokenKind::EqualsEquals)
            }
            '~' => TokenKind::Tilde,
            '<' => {
                if lexer.match_next('<') {
                    lexer.consume();
                    TokenKind::DoubleLessThan
                } else {
                    lexer.lex_potential_double(
                        '=',
                        TokenKind::LessThan,
                        TokenKind::LessThanEquals,
                    )
                }
            }
            '>' => {
                if lexer.match_next('>') {
                    lexer.consume();
                    TokenKind::DoubleGreaterThan
                } else {
                    lexer.lex_potential_double(
                        '=',
                        TokenKind::GreaterThan,
                        TokenKind::GreaterThanEquals,
                    )
                }
            }
            '?' => TokenKind::QuestionMark,
            '&' => lexer.lex_potential_double('&', TokenKind::Ampersand, TokenKind::And),
            '|' => lexer.lex_potential_double('|', TokenKind::Pipe, TokenKind::Or),
            _ => {
                return Err(invalid_token(
                    c.to_string(),
                    TextSpan::new(lexer.position, lexer.position, c.to_string()),
                )
                    .into())
            }
        };

        lexer.consume();

        Ok(kind)
    }
}
use anyhow::Result;
use crate::lexer::Lexer;
use crate::lexer::tokens::TokenKind;

/// The type of number.
#[derive(Debug)]
pub enum NumberType {
    Integer(i64),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteral {}

impl NumberLiteral {
    pub fn lex_number(lexer: &mut Lexer, c: char) -> Result<TokenKind> {
        let mut number = NumberType::Integer(0);

        lexer.consume();

        let consume_num = |mut num_str: String, lexer: &mut Lexer, radix: u32| -> Result<String>{
            while let Some(c) = lexer.current() {
                if c.is_digit(radix) {
                    num_str.push(c);
                    lexer.consume();
                } else {
                    break;
                }
            }
            Ok(num_str)
        };

        if c == '0' {
            if let Some(c) = lexer.peek() {
                if matches!(c, 'x' | 'X') {
                    // Hexadecimal
                    lexer.consume();
                    number = NumberType::Integer(i64::from_str_radix(&consume_num(String::new(), lexer, 16)?, 16)?);
                } else if matches!(c, 'o' | 'O') {
                    // Octal
                    lexer.consume();
                    number = NumberType::Integer(i64::from_str_radix(&consume_num(String::new(), lexer, 8)?, 8)?);
                } else if matches!(c, 'b' | 'B') {
                    // Binary
                    lexer.consume();
                    let mut bin = String::new();
                    while let Some(c) = lexer.current() {
                        if c == '0' || c == '1' {
                            bin.push(c);
                            lexer.consume();
                        } else {
                            break;
                        }
                    }
                    number = NumberType::Integer(i64::from_str_radix(&bin, 2)?);
                } else if c.is_digit(10) {
                    let mut num_str = String::from("0");
                    num_str.push(c);
                    lexer.consume();

                    number = NumberType::Integer(
                        consume_num(num_str, lexer, 10)?.parse()?
                    );
                } else {
                    // Invalid base specifier, return '0'
                    return Ok(TokenKind::Integer(0));
                }
            }
        } else {
            let mut num_str = String::new();
            num_str.push(c);

            while let Some(c) = lexer.current() {
                if c.is_digit(10) {
                    num_str.push(c);
                    lexer.consume();
                } else if c == '.' {
                    num_str.push(c);
                    lexer.consume();
                    while let Some(c) = lexer.current() {
                        if c.is_digit(10) {
                            num_str.push(c);
                            lexer.consume();
                        } else {
                            break;
                        }
                    }
                    number = NumberType::Float(num_str.parse()?);
                    break;
                } else {
                    break;
                }
            }

            if let NumberType::Integer(_) = number {
                number = NumberType::Integer(num_str.parse()?);
            }
        }


        Ok(match number {
            NumberType::Integer(i) => TokenKind::Integer(i),
            NumberType::Float(f) => TokenKind::Float(f),
        })
    }
}

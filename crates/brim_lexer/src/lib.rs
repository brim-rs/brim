//! Implementation is based on rustc_lexer

use crate::{
    LiteralKind::{Byte, ByteStr, Float, Int},
    PrimitiveTokenKind::*,
    cursor::Cursor,
    idents::{is_identifier_start, is_valid_ident_continue},
    whitespace::is_whitespace,
};
use std::cmp::PartialEq;
use std::fmt::Display;
use unicode_properties::UnicodeEmoji;

pub mod cursor;
pub mod idents;
pub mod whitespace;

#[derive(Debug, Clone)]
pub struct PrimitiveToken {
    pub kind: PrimitiveTokenKind,
    pub len: u32,
}

impl PrimitiveToken {
    pub fn new(kind: PrimitiveTokenKind, len: u32) -> Self {
        Self { kind, len }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RawStrError {
    InvalidStarter {
        bad_char: char,
    },
    NoTerminator {
        expected: u32,
        found: u32,
        possible_terminator_offset: Option<u32>,
    },
    TooManyDelimiters {
        found: u32,
    },
}

#[cfg(debug_assertions)]
impl PartialEq for PrimitiveToken {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
    /// Literal starts with "0b".
    Binary = 2,
    /// Literal starts with "0o".
    Octal = 8,
    /// Literal doesn't contain a prefix.
    Decimal = 10,
    /// Literal starts with "0x".
    Hexadecimal = 16,
}

impl Display for Base {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Base::Binary => write!(f, "binary"),
            Base::Octal => write!(f, "octal"),
            Base::Decimal => write!(f, "decimal"),
            Base::Hexadecimal => write!(f, "hexadecimal"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    /// Integer literals with optional suffix
    /// Examples: `123`, `0b101`, `0xFF_u8`
    Int { base: Base, empty_int: bool },
    /// Float literals with type suffix
    /// Examples: `1.23`, `1e5`, `1.5f32`
    Float { base: Base, empty_exponent: bool },
    /// Character literals in single quotes
    /// Examples: `'a'`, `'\n'`, `'🧪'`
    Char { terminated: bool },
    /// Byte literals with b-prefix
    /// Examples: `b'x'`, `b'\n'`, `b'!'`
    Byte { terminated: bool },
    /// String literals in double quotes
    /// Examples: `"hello"`, `"rust\n"`
    Str { terminated: bool },
    /// Byte string literals with b-prefix
    /// Examples: `b"bytes"`, `b"hello\n"`
    ByteStr { terminated: bool },
    /// C-compatible string literals with c-prefix
    /// Examples: `c"hello"`, `c"null-terminated"`
    CStr { terminated: bool },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveTokenKind {
    /// Identifier or keyword
    Ident,
    /// Any valid whitespace
    Whitespace,
    /// Literals. eg:
    Literal {
        kind: LiteralKind,
        suffix_start: u32,
    },

    /// Comment, doc or plain
    Comment {
        doc: bool,
    },

    /// +
    Plus,
    /// -
    Minus,
    /// *
    Asterisk,
    /// /
    Slash,
    /// =
    Equals,
    /// &
    Ampersand,
    /// |
    Pipe,
    /// ^
    Caret,
    /// %
    Percent,
    /// ~
    Tilde,
    /// !
    Bang,
    /// ?
    QuestionMark,
    /// @
    At,
    /// >
    GreaterThan,
    /// <
    LessThan,
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// {
    OpenBrace,
    /// }
    CloseBrace,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// ,
    Comma,
    /// .
    Dot,
    /// :
    Colon,
    /// ;
    Semicolon,
    /// $
    Dollar,

    /// End of file
    Eof,

    /// Unknown token
    Unknown,
    InvalidIdent,
}

impl<'a> Cursor<'a> {
    pub fn next_token(&mut self) -> PrimitiveToken {
        let first_char = match self.bump() {
            Some(c) => c,
            None => return PrimitiveToken::new(PrimitiveTokenKind::Eof, 0),
        };
        let token_kind = match first_char {
            c if is_whitespace(first_char) => {
                self.eat_while(is_whitespace);
                Whitespace
            }

            '/' => match self.first() {
                '/' => self.comment(),
                _ => Slash,
            },

            '+' => Plus,
            '-' => Minus,
            '*' => Asterisk,
            '=' => Equals,
            '&' => Ampersand,
            '|' => Pipe,
            '^' => Caret,
            '%' => Percent,
            '~' => Tilde,
            '!' => Bang,
            '?' => QuestionMark,
            '@' => At,
            '>' => GreaterThan,
            '<' => LessThan,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            ',' => Comma,
            '.' => Dot,
            ':' => Colon,
            ';' => Semicolon,
            '$' => Dollar,

            c @ '0'..='9' => {
                let literal_kind = self.number(c);
                let suffix_start = self.pos_within_token();
                self.eat_literal_suffix();
                PrimitiveTokenKind::Literal {
                    kind: literal_kind,
                    suffix_start,
                }
            }

            'b' => match self.first() {
                '\'' => {
                    self.bump();
                    let terminated = self.single_quoted_string();
                    let suffix_start = self.pos_within_token();
                    if terminated {
                        self.eat_identifier();
                    }
                    Literal {
                        kind: Byte { terminated },
                        suffix_start,
                    }
                }
                '"' => {
                    self.bump();
                    let terminated = self.double_quoted_string();
                    let suffix_start = self.pos_within_token();
                    if terminated {
                        self.eat_identifier();
                    }
                    Literal {
                        kind: ByteStr { terminated },
                        suffix_start,
                    }
                }
                _ => self.ident_or_unknown_prefix(),
            },

            'c' => match self.first() {
                '"' => {
                    self.bump();
                    let terminated = self.double_quoted_string();
                    let suffix_start = self.pos_within_token();
                    if terminated {
                        self.eat_identifier();
                    }
                    Literal {
                        kind: LiteralKind::CStr { terminated },
                        suffix_start,
                    }
                }
                _ => self.ident_or_unknown_prefix(),
            },

            '"' => {
                self.bump();
                let terminated = self.double_quoted_string();
                let suffix_start = self.pos_within_token();
                if terminated {
                    self.eat_identifier();
                }
                Literal {
                    kind: LiteralKind::Str { terminated },
                    suffix_start,
                }
            }
            '\'' => {
                let terminated = self.single_quoted_string();
                let suffix_start = self.pos_within_token();
                if terminated {
                    self.eat_identifier();
                }
                Literal {
                    kind: LiteralKind::Char { terminated },
                    suffix_start,
                }
            }

            c if is_identifier_start(c) => self.ident_or_unknown_prefix(),

            _ => Unknown,
        };

        let token = PrimitiveToken {
            kind: token_kind,
            len: self.pos_within_token(),
        };

        self.reset_pos_within_token();
        token
    }

    fn number(&mut self, first_digit: char) -> LiteralKind {
        let mut base = Base::Decimal;
        if first_digit == '0' {
            match self.first() {
                'b' => {
                    base = Base::Binary;
                    self.bump();
                    if !self.eat_decimal_digits() {
                        return Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                'o' => {
                    base = Base::Octal;
                    self.bump();
                    if !self.eat_decimal_digits() {
                        return Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.bump();
                    if !self.eat_hexadecimal_digits() {
                        return Int {
                            base,
                            empty_int: true,
                        };
                    }
                }
                '0'..='9' | '_' => {
                    self.eat_decimal_digits();
                }

                '.' | 'e' | 'E' => {}

                _ => {
                    return Int {
                        base,
                        empty_int: false,
                    };
                }
            }
        } else {
            self.eat_decimal_digits();
        };

        match self.first() {
            '.' if self.second() != '.' && !is_identifier_start(self.second()) => {
                self.bump();
                let mut empty_exponent = false;
                if self.first().is_ascii_digit() {
                    self.eat_decimal_digits();
                    match self.first() {
                        'e' | 'E' => {
                            self.bump();
                            empty_exponent = !self.eat_float_exponent();
                        }
                        _ => (),
                    }
                }
                Float {
                    base,
                    empty_exponent,
                }
            }
            'e' | 'E' => {
                self.bump();
                let empty_exponent = !self.eat_float_exponent();
                Float {
                    base,
                    empty_exponent,
                }
            }
            _ => Int {
                base,
                empty_int: false,
            },
        }
    }

    pub fn comment(&mut self) -> PrimitiveTokenKind {
        self.bump();
        let mut doc_comment = false;

        match self.first() {
            '/' => {
                doc_comment = true;
            }
            _ => {}
        }

        self.eat_while(|c| c != '\n');
        Comment { doc: doc_comment }
    }

    pub fn single_quoted_string(&mut self) -> bool {
        if self.second() == '\'' && self.first() != '\\' {
            self.bump();
            self.bump();
            return true;
        }

        while !self.is_eof() {
            match self.first() {
                '\'' => {
                    self.bump();
                    return true;
                }
                '\\' => {
                    self.bump(); // Skip escape char
                    self.bump(); // Skip next char
                }
                '/' | '\n' => return false,
                _ => {
                    self.bump();
                }
            }
        }
        false
    }

    fn ident_or_unknown_prefix(&mut self) -> PrimitiveTokenKind {
        self.eat_while(is_valid_ident_continue);
        match self.first() {
            c if !c.is_ascii() && c.is_emoji_char() => self.invalid_ident(),
            _ => Ident,
        }
    }

    fn invalid_ident(&mut self) -> PrimitiveTokenKind {
        self.eat_while(|c| {
            const ZERO_WIDTH_JOINER: char = '\u{200d}';
            is_valid_ident_continue(c)
                || (!c.is_ascii() && c.is_emoji_char())
                || c == ZERO_WIDTH_JOINER
        });
        InvalidIdent
    }

    pub fn double_quoted_string(&mut self) -> bool {
        while let Some(c) = self.bump() {
            match c {
                '"' => return true,
                '\\' if self.first() == '\\' || self.first() == '"' => {
                    self.bump();
                }
                _ => (),
            }
        }
        false
    }

    fn eat_identifier(&mut self) {
        if !is_identifier_start(self.first()) {
            return;
        }
        self.bump();

        self.eat_while(is_valid_ident_continue);
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.bump();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.bump();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_float_exponent(&mut self) -> bool {
        if self.first() == '-' || self.first() == '+' {
            self.bump();
        }
        self.eat_decimal_digits()
    }

    fn eat_literal_suffix(&mut self) {
        self.eat_identifier();
    }
}

pub fn tokenize(input: &str) -> impl Iterator<Item = PrimitiveToken> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.next_token();
        if token.kind != PrimitiveTokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}

pub fn tokens_no_whitespace(input: &str) -> impl Iterator<Item = PrimitiveToken> + '_ {
    tokenize(input).filter(|t| t.kind != Whitespace)
}

pub fn tokens(input: &str, whitespace: bool) -> Vec<PrimitiveToken> {
    if whitespace {
        tokenize(input).collect()
    } else {
        tokens_no_whitespace(input).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::LiteralKind::Str;

    #[test]
    fn test_tokenize() {
        let input = "fn main() { println(\"Hello, world!\"); }";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [
            Ident,
            Ident,
            OpenParen,
            CloseParen,
            OpenBrace,
            Ident,
            OpenParen,
            Literal {
                kind: Str { terminated: true },
                suffix_start: 15
            },
            CloseParen,
            Semicolon,
            CloseBrace
        ]);
    }

    #[test]
    fn test_byte_string() {
        let input = "b\"hello\"";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: ByteStr { terminated: true },
            suffix_start: 8
        }]);
    }

    #[test]
    fn test_byte_string_escaped() {
        let input = "b\"hello\\n\"";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: ByteStr { terminated: true },
            suffix_start: 10
        }]);
    }

    #[test]
    fn test_str() {
        let input = "\"hello\"";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: Str { terminated: true },
            suffix_start: 7
        }]);
    }

    #[test]
    fn test_str_escaped() {
        let input = "\"hello\\n\"";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: Str { terminated: true },
            suffix_start: 9
        }]);
    }

    #[test]
    fn test_char() {
        let input = "'a'";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: LiteralKind::Char { terminated: true },
            suffix_start: 3
        }]);
    }

    #[test]
    fn test_char_escaped() {
        let input = "'\\n'";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: LiteralKind::Char { terminated: true },
            suffix_start: 4
        }]);
    }

    #[test]
    fn test_c_str() {
        let input = "c\"hello\"";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: LiteralKind::CStr { terminated: true },
            suffix_start: 8
        }]);
    }

    #[test]
    fn test_c_str_escaped() {
        let input = "c\"hello\\n\"";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: LiteralKind::CStr { terminated: true },
            suffix_start: 10
        }]);
    }

    #[test]
    fn test_int() {
        let input = "123";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: Int {
                base: Base::Decimal,
                empty_int: false
            },
            suffix_start: 3
        }]);
    }

    #[test]
    fn test_int_bin() {
        let input = "0b101";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: Int {
                base: Base::Binary,
                empty_int: false
            },
            suffix_start: 5
        }]);
    }

    #[test]
    fn test_int_octal() {
        let input = "0o123";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: Int {
                base: Base::Octal,
                empty_int: false
            },
            suffix_start: 5
        }]);
    }

    #[test]
    fn test_int_hex() {
        let input = "0x123abc";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: Int {
                base: Base::Hexadecimal,
                empty_int: false
            },
            suffix_start: 8
        }]);
    }

    #[test]
    fn test_float() {
        let input = "1.23";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: Float {
                base: Base::Decimal,
                empty_exponent: false
            },
            suffix_start: 4
        }]);
    }

    #[test]
    fn test_float_exp() {
        let input = "1.23e5";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: Float {
                base: Base::Decimal,
                empty_exponent: false
            },
            suffix_start: 6
        }]);
    }

    #[test]
    fn test_float_exp_neg() {
        let input = "1.23e-5";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: Float {
                base: Base::Decimal,
                empty_exponent: false
            },
            suffix_start: 7
        }]);
    }

    #[test]
    fn test_float_exp_pos() {
        let input = "1.23e+5";
        let tokens: Vec<_> = tokens(input, false).iter().map(|t| t.kind).collect();
        assert_eq!(tokens, [Literal {
            kind: Float {
                base: Base::Decimal,
                empty_exponent: false
            },
            suffix_start: 7
        }]);
    }
}

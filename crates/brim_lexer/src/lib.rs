//! Implementation is based on rustc_lexer

use crate::{
    PrimitiveTokenKind::*,
    cursor::Cursor,
    idents::{is_identifier_start, is_valid_ident_continue},
    whitespace::is_whitespace,
};
use std::cmp::PartialEq;
use unicode_properties::UnicodeEmoji;

mod cursor;
mod idents;
mod whitespace;

#[derive(Debug)]
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
    /// Raw string literals using $, with optional nesting level
    /// Examples: `$hello$`, `$2$nested text$2$`
    RawStr { n_level: Option<u8> },
    /// Raw byte string literals with b-prefix and $ delimiters
    /// Examples: `b$"raw bytes"$`, `b$2$"raw bytes"$2$`
    RawByteStr { n_level: Option<u8> },
    /// Raw C string literals with c-prefix and $ delimiters
    /// Examples: `c$raw c string$`, `c$2$raw c string$2$`
    RawCStr { n_level: Option<u8> },
}

/// Multiple-character symbols are made later in the [Lexer](crate::lexer::Lexer)
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
    UnknownPrefix,
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

            '"' => {
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
            '#' | '"' | '\'' => UnknownPrefix,
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
}

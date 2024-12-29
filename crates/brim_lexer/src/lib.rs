use crate::cursor::Cursor;
use crate::PrimitiveTokenKind::*;
use crate::whitespace::is_whitespace;

mod cursor;
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

/// Multiple-character symbols are made later in the [Lexer](crate::lexer::Lexer)
#[derive(Debug)]
pub enum PrimitiveTokenKind {
    /// Identifier or keyword
    Ident,
    /// Any valid whitespace
    Whitespace,
    /// Literals. eg:
    Literal { kind: LiteralKind, suffix_start: u32 },

    /// Comment, doc or plain
    Comment { doc: bool },

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

    /// End of file
    Eof,
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
                _ => Slash
            }

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
}
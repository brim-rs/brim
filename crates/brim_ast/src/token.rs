use std::cmp::PartialEq;
use std::fmt::Display;
use crate::ErrorEmitted;
use brim_span::{span::Span, symbols::Symbol};
use crate::item::Ident;

#[derive(Clone, PartialEq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn as_comment(&self) -> &Symbol {
        match &self.kind {
            TokenKind::DocComment(comment) => comment,
            _ => panic!("Token is not a comment"),
        }
    }

    pub fn as_ident(&self) -> Option<Ident> {
        match &self.kind {
            TokenKind::Ident(ident) => Some(Ident::new(*ident, self.span)),
            _ => None,
        }
    }

    pub fn is_keyword(&self, sym: Symbol) -> bool {
        match self.as_ident() {
            Some(ident) => ident.name == sym,
            None => false,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Delimiter {
    /// `( ... )`
    Paren,
    /// `{ ... }`
    Brace,
    /// `[ ... ]`
    Bracket,
}

#[derive(PartialEq, Debug, Clone, Eq)]
pub enum Orientation {
    Open,
    Close,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
    // Symbols
    Delimiter(Delimiter, Orientation),
    /// `;`
    Colon,
    /// `,`
    Comma,
    /// `@`
    At,
    /// `.`
    Dot,
    /// `->`
    Arrow,
    /// `$`
    Dollar,
    /// `;`
    Semicolon,
    /// `?`
    QuestionMark,
    /// `!`
    Bang,

    // Operators
    /// `=`
    Eq,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `==`
    EqEq,
    /// `!=`
    Ne,
    /// `>=`
    Ge,
    /// `>`
    Gt,
    /// `&&`
    AndAnd,
    /// `||`
    OrOr,
    /// `~`
    Tilde,

    Literal(Lit),

    /// Identifier
    Ident(Symbol),

    /// Binary operator
    BinOp(BinOpToken),

    Skipable,
    DocComment(Symbol),

    Eof,
}

#[derive(Clone, PartialEq, Hash, Debug, Copy)]
pub enum BinOpToken {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    And,
    Or,
    ShiftLeft,
    ShiftRight,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: Symbol,
    pub suffix: Option<Symbol>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum LitKind {
    Bool,
    Integer,
    Float,
    Char,
    Byte,
    Str,
    ByteStr,
    CStr,
    Err(ErrorEmitted),
}

impl Lit {
    pub fn new(kind: LitKind, symbol: Symbol, suffix: Option<Symbol>) -> Self {
        Self {
            kind,
            symbol,
            suffix,
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.symbol)
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Delimiter(delimiter, orientation) => {
                let symbol = match delimiter {
                    Delimiter::Paren => if orientation == &Orientation::Open { "(" } else { ")" },
                    Delimiter::Brace => if orientation == &Orientation::Open { "{" } else { "}" },
                    Delimiter::Bracket => if orientation == &Orientation::Open { "[" } else { "]" },
                };
                write!(f, "{}", symbol)
            }
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::At => write!(f, "@"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Dollar => write!(f, "$"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::QuestionMark => write!(f, "?"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Le => write!(f, "<="),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::Ne => write!(f, "!="),
            TokenKind::Ge => write!(f, ">="),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::AndAnd => write!(f, "&&"),
            TokenKind::OrOr => write!(f, "||"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::Literal(lit) => write!(f, "{}", lit),
            TokenKind::Ident(ident) => write!(f, "{}", ident),
            TokenKind::BinOp(bin_op) => write!(f, "{:?}", bin_op),
            TokenKind::Skipable => write!(f, "Skipable"),
            TokenKind::DocComment(comment) => write!(f, "DocComment({})", comment),
            TokenKind::Eof => write!(f, "EOF"),
        }
    }
}
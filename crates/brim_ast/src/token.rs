use crate::{ErrorEmitted, item::Ident};
use brim_span::{span::Span, symbols::Symbol};
use std::{cmp::PartialEq, fmt::Display};

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

    pub fn is_ident(&self, func: impl FnOnce(Ident) -> bool) -> bool {
        match self.as_ident() {
            Some(ident) => func(ident),
            None => false,
        }
    }

    pub fn is_keyword(&self, sym: Symbol) -> bool {
        self.is_ident(|ident| ident.name == sym)
    }

    pub fn is_any_keyword(&self) -> bool {
        self.is_ident(|ident| ident.is_reserved())
    }

    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }

    pub fn is_delimiter(&self, delimiter: Delimiter, orientation: Orientation) -> bool {
        match &self.kind {
            TokenKind::Delimiter(d, o) => d == &delimiter && o == &orientation,
            _ => false,
        }
    }

    /// Check if the token is an assignment operator other than `=`, eg: `+=`, `-=`, etc.
    pub fn is_compound_assign(&self) -> Option<AssignOpToken> {
        match &self.kind {
            TokenKind::AssignOp(op) => Some(*op),
            _ => None,
        }
    }

    pub fn is_assign(&self) -> bool {
        self.is(TokenKind::Eq)
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

#[derive(PartialEq, Debug, Clone, Eq, Copy)]
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
    /// `::`
    DoubleColon,

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
    AssignOp(AssignOpToken),

    Skipable,
    DocComment(Symbol),

    Eof,
}

#[derive(Clone, PartialEq, Hash, Debug, Copy)]
pub enum AssignOpToken {
    /// `+=`
    PlusEq,
    /// `-=`
    MinusEq,
    /// `*=`
    StarEq,
    /// `/=`
    SlashEq,
    /// `%=`
    ModEq,
    /// `^=`
    CaretEq,
    /// `&=`
    AndEq,
    /// `|=`
    OrEq,
    /// `<<=`
    ShlEq,
    /// `>>=`
    ShrEq,
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
    Power,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: Ident,
    pub suffix: Option<Ident>,
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
    pub fn new(kind: LitKind, symbol: Ident, suffix: Option<Ident>) -> Self {
        Self {
            kind,
            symbol,
            suffix,
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.symbol,
            self.suffix.map_or("".to_string(), |s| s.to_string())
        )
    }
}

impl Display for AssignOpToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignOpToken::PlusEq => write!(f, "+="),
            AssignOpToken::MinusEq => write!(f, "-="),
            AssignOpToken::StarEq => write!(f, "*="),
            AssignOpToken::SlashEq => write!(f, "/="),
            AssignOpToken::ModEq => write!(f, "%="),
            AssignOpToken::CaretEq => write!(f, "^="),
            AssignOpToken::AndEq => write!(f, "&="),
            AssignOpToken::OrEq => write!(f, "|="),
            AssignOpToken::ShlEq => write!(f, "<<="),
            AssignOpToken::ShrEq => write!(f, ">>="),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Delimiter(delimiter, orientation) => {
                let symbol = match delimiter {
                    Delimiter::Paren => {
                        if orientation == &Orientation::Open {
                            "("
                        } else {
                            ")"
                        }
                    }
                    Delimiter::Brace => {
                        if orientation == &Orientation::Open {
                            "{"
                        } else {
                            "}"
                        }
                    }
                    Delimiter::Bracket => {
                        if orientation == &Orientation::Open {
                            "["
                        } else {
                            "]"
                        }
                    }
                };
                write!(f, "{}", symbol)
            }
            TokenKind::AssignOp(assign_op) => write!(f, "{}", assign_op),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::At => write!(f, "@"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Dollar => write!(f, "$"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::QuestionMark => write!(f, "?"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::DoubleColon => write!(f, "::"),
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

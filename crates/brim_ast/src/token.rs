use brim_span::{span::Span, symbol::Symbol};

#[derive(Clone, PartialEq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
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

#[derive(PartialEq, Debug, Clone)]
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

use brim_span::span::Span;

#[derive(Clone, PartialEq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Delimeter {
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
    Delimeter(Delimeter, Orientation),
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

    // Literal(Lit),
}

// #[derive(Clone, Copy, PartialEq, Debug)]
// pub struct Lit {
//     pub kind: LitKind,
//     pub symbol: Symbol,
//     pub suffix: Option<Symbol>,
// }
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

#[derive(Debug)]
pub enum PrimitiveTokenKind {
    /// Identifier or keyword
    Ident,
    /// Any valid whitespace
    Whitespace,
    /// Literals. eg:
    Literal { kind: LiteralKind, suffix_start: u32 },
}
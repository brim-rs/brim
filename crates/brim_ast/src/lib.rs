#![allow(non_upper_case_globals)]

use brim_span::symbols::Symbol;
use brim_span::symbols::SymbolIndex;
use brim_index::index_type;

pub mod expr;
pub mod item;
mod stmts;
pub mod token;
pub mod ty;


use brim_symbols_macro::generate_symbols;
use indexmap::IndexMap;
use lazy_static::lazy_static;

generate_symbols! {
    Break = "break",
    Catch = "catch",
    Const = "const",
    Continue = "continue",
    Else = "else",
    Enum = "enum",
    False = "false",
    Fn = "fn",
    For = "for",
    From = "from",
    If = "if",
    Impl = "impl",
    In = "in",
    Let = "let",
    Loop = "loop",
    Null = "null",
    Pub = "pub",
    Return = "return",
    SelfBig = "Self",
    SelfSmall = "Small",
    Struct = "struct",
    Then = "then",
    Trait = "trait",
    True = "true",
    Use = "use",
    While = "while",
}


/// A struct that represents already emitted diagnostic
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ErrorEmitted(());

impl ErrorEmitted {
    pub fn new() -> Self {
        Self(())
    }
}

index_type! {
    /// A unique identifier for a node in the AST.
    pub struct NodeId {}
}

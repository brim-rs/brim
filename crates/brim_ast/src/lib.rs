#![allow(non_upper_case_globals)]

use brim_index::index_type;
use brim_span::symbols::{Symbol, SymbolIndex};

pub mod expr;
pub mod item;
pub mod stmts;
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
    Comptime = "comptime",
    Else = "else",
    Enum = "enum",
    Extern = "extern",
    False = "false",
    Fn = "fn",
    For = "for",
    From = "from",
    If = "if",
    In = "in",
    Let = "let",
    Loop = "loop",
    Null = "null",
    Match = "match",
    Mod = "mod",
    Mut = "mut",
    Parent = "parent",
    Pub = "pub",
    Return = "return",
    Struct = "struct",
    Then = "then",
    Trait = "trait",
    True = "true",
    Try = "try",
    Type = "type",
    Use = "use",
    While = "while",

    SelfBig = "Self",
    SelfSmall = "self",
    Empty = "",
}

index_type! {
    /// A unique identifier for a node in the AST.
    pub struct ItemId {}
}

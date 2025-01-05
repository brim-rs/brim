#![allow(non_upper_case_globals)]

use brim::symbols::{Symbol, SymbolIndex};
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

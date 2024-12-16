//!! List of primitives types in Brim:
//! | Type    | Description                    |
//! |---------|--------------------------------|
//! | `i8`    | 8-bit signed integer           |
//! | `i16`   | 16-bit signed integer          |
//! | `i32`   | 32-bit signed integer          |
//! | `i64`   | 64-bit signed integer          |
//! | `i128`  | 128-bit signed integer         |
//! | `u8`    | 8-bit unsigned integer         |
//! | `u16`   | 16-bit unsigned integer        |
//! | `u32`   | 32-bit unsigned integer        |
//! | `u64`   | 64-bit unsigned integer        |
//! | `u128`  | 128-bit unsigned integer       |
//! | `usize` | Pointer-sized unsigned integer |
//! | `isize` | Pointer-sized signed integer   |
//! | `f16`   | 16-bit floating-point number   |
//! | `f32`   | 32-bit floating-point number   |
//! | `f64`   | 64-bit floating-point number   |
//! | `f128`  | 128-bit floating-point number  |
//! | `bool`  | Boolean. `true` or `false`     |
//! | `void`  | No return value                |
//! | `char`  | Unicode character              |
//!
//! Brim also has types like `undefined` and `null`.
//!
//! Look at `design.md` for more information.

use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    Isize,
    F16,
    F32,
    F64,
    F128,
    Bool,
    Void,
    Char,
    Undefined,
    Null,
    String,
    Array(Box<TypeKind>),
    Custom(String),
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::I8 => write!(f, "i8"),
            TypeKind::I16 => write!(f, "i16"),
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::I128 => write!(f, "i128"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U16 => write!(f, "u16"),
            TypeKind::U32 => write!(f, "u32"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::U128 => write!(f, "u128"),
            TypeKind::Usize => write!(f, "usize"),
            TypeKind::Isize => write!(f, "isize"),
            TypeKind::F16 => write!(f, "f16"),
            TypeKind::F32 => write!(f, "f32"),
            TypeKind::F64 => write!(f, "f64"),
            TypeKind::F128 => write!(f, "f128"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Void => write!(f, "void"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::Undefined => write!(f, "undefined"),
            TypeKind::Null => write!(f, "null"),
            TypeKind::String => write!(f, "string"),
            TypeKind::Custom(name) => write!(f, "{}", name),
            TypeKind::Array(t) => write!(f, "{}[]", t),
        }
    }
}

impl TypeKind {
    pub fn from_str(s: &str, is_array: bool) -> Self {
        if is_array {
            return TypeKind::Array(Box::new(TypeKind::from_str(s, false)));
        }
        
        match s {
            "i8" => TypeKind::I8,
            "i16" => TypeKind::I16,
            "i32" => TypeKind::I32,
            "i64" => TypeKind::I64,
            "i128" => TypeKind::I128,
            "u8" => TypeKind::U8,
            "u16" => TypeKind::U16,
            "u32" => TypeKind::U32,
            "u64" => TypeKind::U64,
            "u128" => TypeKind::U128,
            "usize" => TypeKind::Usize,
            "isize" => TypeKind::Isize,
            "f16" => TypeKind::F16,
            "f32" => TypeKind::F32,
            "f64" => TypeKind::F64,
            "f128" => TypeKind::F128,
            "bool" => TypeKind::Bool,
            "void" => TypeKind::Void,
            "char" => TypeKind::Char,
            "undefined" => TypeKind::Undefined,
            "null" => TypeKind::Null,
            "string" => TypeKind::String,
            _ => TypeKind::Custom(s.to_string()),
        }
    }
}
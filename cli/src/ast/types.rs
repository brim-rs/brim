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
//! | `f32`   | 32-bit floating-point number   |
//! | `f64`   | 64-bit floating-point number   |
//! | `bool`  | Boolean. `true` or `false`     |
//! | `void`  | No return value                |
//! | `char`  | Unicode character              |
//!
//! Brim also has types like `undefined` and `null`.
//!
//! Look at `design.md` for more information.

use crate::compilation::passes::type_checker::ResolvedType;
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
    F32,
    F64,
    Bool,
    Void,
    Char,
    Undefined,
    Null,
    String,
    Array(Box<ResolvedType>),
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
            TypeKind::F32 => write!(f, "f32"),
            TypeKind::F64 => write!(f, "f64"),
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
            return TypeKind::Array(Box::new(ResolvedType::base(TypeKind::from_str(s, false))));
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
            "f32" => TypeKind::F32,
            "f64" => TypeKind::F64,
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

use std::cmp::Ordering;

impl PartialOrd for TypeKind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for TypeKind {}

impl Ord for TypeKind {
    fn cmp(&self, other: &Self) -> Ordering {
        use TypeKind::*;
        match (self, other) {
            (Custom(a), Custom(b)) => a.cmp(b),
            (Array(a), Array(b)) => a.kind.cmp(&b.kind),
            (Custom(_), _) => Ordering::Greater,
            (_, Custom(_)) => Ordering::Less,
            (Array(_), _) => Ordering::Greater,
            (_, Array(_)) => Ordering::Less,
            _ => self.to_discriminant().cmp(&other.to_discriminant()),
        }
    }
}

impl TypeKind {
    fn to_discriminant(&self) -> usize {
        use TypeKind::*;
        match self {
            Bool => 0,
            Char => 1,
            F32 => 2,
            F64 => 3,
            I8 => 4,
            I16 => 5,
            I32 => 6,
            I64 => 7,
            I128 => 8,
            Isize => 9,
            Null => 10,
            String => 11,
            U8 => 12,
            U16 => 13,
            U32 => 14,
            U64 => 15,
            U128 => 16,
            Usize => 17,
            Void => 18,
            Undefined => 19,
            Array(_) => 20, // Arrays and custom types are handled separately
            Custom(_) => 21,
        }
    }
}

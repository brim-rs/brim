use crate::{
    ItemId,
    expr::Expr,
    item::{GenericArgs, Ident},
    ty::PrimitiveType::{I8, I16, I32, I64, Isize, U8, U16, U32, U64, Usize},
};
use brim_diagnostics::ErrorEmitted;
use brim_span::span::Span;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Ty {
    pub id: ItemId,
    pub kind: TyKind,
    pub span: Span,
}

impl Ty {
    pub fn is_const(&self) -> bool {
        match &self.kind {
            TyKind::Mut(_) | TyKind::Ref(_, Mutable::No) | TyKind::Ptr(_, Mutable::No) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Mutable {
    Yes,
    No,
}

impl Mutable {
    pub fn from_bool(is_mutable: bool) -> Self {
        if is_mutable {
            Mutable::Yes
        } else {
            Mutable::No
        }
    }
}

#[derive(Debug, Clone)]
pub enum TyKind {
    /// Reference type eg. `&T` (brim) -> `T&` (C++) or `mut &T` (brim) -> `T&` (C++)
    Ref(Box<Ty>, Mutable),
    /// Pointer type eg. `*T` (brim) -> `T*` (C++) or `mut *T` (brim) -> `T*` (C++)
    Ptr(Box<Ty>, Mutable),
    /// Mutable type eg. `mut T` (brim) -> `T` (C++)
    Mut(Box<Ty>),
    /// Const type eg. `const T` (brim) -> `const T` (C++)
    Const(Box<Ty>),
    /// Vector type eg. `T[]` (brim) -> `std::vector<T>` (C++). Resizable array. The syntax in brim
    /// is the same as array in C++.
    Vec(Box<Ty>),
    /// Primitive type eg. `i32` (brim) -> `int32_t` (C++)
    Primitive(PrimitiveType),
    /// Any other type that can be enum, struct, type, etc.
    Ident { ident: Ident, generics: GenericArgs },
    /// Result type eg. `Result<T, E>` (brim) -> `std::expected<T, E>` (C++)
    Result(Box<Ty>, Box<Ty>),
    /// Option type eg. `T?` (brim) -> `std::optional<T>` (C++)
    Option(Box<Ty>),

    /// Indicating that the compiler failed to determine the type
    Err(ErrorEmitted),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    // Signed integers
    I8,
    I16,
    I32,
    I64,
    Isize,

    // Unsigned integers
    U8,
    U16,
    U32,
    U64,
    Usize,

    // Floating point numbers
    F32,
    F64,

    Bool,
    Char,
    String,
    Void,

    /// Any type `any` (brim) -> `std::any` (C++)
    Any,
}

impl PrimitiveType {
    pub fn try_from_string(s: String) -> Option<Self> {
        match s.as_str() {
            "i8" => Some(PrimitiveType::I8),
            "i16" => Some(PrimitiveType::I16),
            "i32" => Some(PrimitiveType::I32),
            "i64" => Some(PrimitiveType::I64),
            "isize" => Some(PrimitiveType::Isize),

            "u8" => Some(PrimitiveType::U8),
            "u16" => Some(PrimitiveType::U16),
            "u32" => Some(PrimitiveType::U32),
            "u64" => Some(PrimitiveType::U64),
            "usize" => Some(PrimitiveType::Usize),

            "f32" => Some(PrimitiveType::F32),
            "f64" => Some(PrimitiveType::F64),

            "bool" => Some(PrimitiveType::Bool),
            "char" => Some(PrimitiveType::Char),
            "string" => Some(PrimitiveType::String),

            "void" => Some(PrimitiveType::Void),

            "any" => Some(PrimitiveType::Any),
            _ => None,
        }
    }

    pub fn promote_type(left: &PrimitiveType, right: &PrimitiveType) -> Option<PrimitiveType> {
        use PrimitiveType::*;

        match (left, right) {
            // Integer promotion
            (I8, I16) | (I16, I8) => Some(I16),
            (I16, I32) | (I32, I16) => Some(I32),
            (I32, I64) | (I64, I32) => Some(I64),
            (I8, Isize) | (Isize, I8) => Some(Isize),
            (I16, Isize) | (Isize, I16) => Some(Isize),
            (I32, Isize) | (Isize, I32) => Some(Isize),
            (I64, Isize) | (Isize, I64) => Some(I64),

            // Unsigned integer promotion
            (U8, U16) | (U16, U8) => Some(U16),
            (U16, U32) | (U32, U16) => Some(U32),
            (U32, U64) | (U64, U32) => Some(U64),
            (U8, Usize) | (Usize, U8) => Some(Usize),
            (U16, Usize) | (Usize, U16) => Some(Usize),
            (U32, Usize) | (Usize, U32) => Some(Usize),
            (U64, Usize) | (Usize, U64) => Some(U64),

            // Mixed signed and unsigned integer promotion
            // I8 with unsigned types
            (I8, U8) | (U8, I8) => Some(I16),
            (I8, U16) | (U16, I8) => Some(U16),
            (I8, U32) | (U32, I8) => Some(U32),
            (I8, U64) | (U64, I8) => Some(U64),
            (I8, Usize) | (Usize, I8) => Some(Usize),

            // I16 with unsigned types
            (I16, U8) | (U8, I16) => Some(I16),
            (I16, U16) | (U16, I16) => Some(I32),
            (I16, U32) | (U32, I16) => Some(U32),
            (I16, U64) | (U64, I16) => Some(U64),
            (I16, Usize) | (Usize, I16) => Some(Usize),

            // I32 with unsigned types
            (I32, U8) | (U8, I32) => Some(I32),
            (I32, U16) | (U16, I32) => Some(I32),
            (I32, U32) | (U32, I32) => Some(I64),
            (I32, U64) | (U64, I32) => Some(U64),
            (I32, Usize) | (Usize, I32) => Some(Usize),

            // I64 with unsigned types
            (I64, U8) | (U8, I64) => Some(I64),
            (I64, U16) | (U16, I64) => Some(I64),
            (I64, U32) | (U32, I64) => Some(I64),
            (I64, U64) | (U64, I64) => Some(U64),
            (I64, Usize) | (Usize, I64) => Some(Usize),

            // Isize with unsigned types
            (Isize, U8) | (U8, Isize) => Some(Isize),
            (Isize, U16) | (U16, Isize) => Some(Isize),
            (Isize, U32) | (U32, Isize) => Some(Isize),
            (Isize, U64) | (U64, Isize) => Some(U64),
            (Isize, Usize) | (Usize, Isize) => Some(Usize),

            // Mixed integer and float promotion
            (I8, F32) | (F32, I8) => Some(F32),
            (I16, F32) | (F32, I16) => Some(F32),
            (I32, F32) | (F32, I32) => Some(F32),
            (I64, F32) | (F32, I64) => Some(F32),
            (U8, F32) | (F32, U8) => Some(F32),
            (U16, F32) | (F32, U16) => Some(F32),
            (U32, F32) | (F32, U32) => Some(F32),
            (U64, F32) | (F32, U64) => Some(F32),

            (I8, F64) | (F64, I8) => Some(F64),
            (I16, F64) | (F64, I16) => Some(F64),
            (I32, F64) | (F64, I32) => Some(F64),
            (I64, F64) | (F64, I64) => Some(F64),
            (U8, F64) | (F64, U8) => Some(F64),
            (U16, F64) | (F64, U16) => Some(F64),
            (U32, F64) | (F64, U32) => Some(F64),
            (U64, F64) | (F64, U64) => Some(F64),

            (Isize, F32) | (F32, Isize) => Some(F32),
            (Isize, F64) | (F64, Isize) => Some(F64),
            (Usize, F32) | (F32, Usize) => Some(F32),
            (Usize, F64) | (F64, Usize) => Some(F64),

            // Float promotion
            (F32, F64) | (F64, F32) => Some(F64),

            // Same types
            (l, r) if l == r => Some(l.clone()),

            // String promotion
            (String, Char) | (Char, String) => Some(String),

            // Default case
            _ => None,
        }
    }

    pub fn can_initialize_with_type(source_ty: &PrimitiveType, target_ty: &PrimitiveType) -> bool {
        use PrimitiveType::*;

        match (source_ty, target_ty) {
            (I32, U8) => true,
            (I32, U16) => true,
            (I32, U32) => true,
            (I32, U64) => true,
            (I32, Usize) => true,

            (I8, U8) => true,
            (I16, U16) => true,
            (I64, U64) => true,
            (Isize, Usize) => true,

            _ => PrimitiveType::promote_type(source_ty, target_ty).is_some(),
        }
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use PrimitiveType::*;

        let s = match self {
            I8 => "i8",
            I16 => "i16",
            I32 => "i32",
            I64 => "i64",
            Isize => "isize",

            U8 => "u8",
            U16 => "u16",
            U32 => "u32",
            U64 => "u64",
            Usize => "usize",

            F32 => "f32",
            F64 => "f64",

            Bool => "bool",
            Char => "char",
            String => "str",
            Void => "void",

            Any => "any",
        };

        write!(f, "{}", s)
    }
}

use crate::{
    ErrorEmitted, NodeId,
    expr::ConstExpr,
    item::{Generics, Ident},
};
use brim_span::span::Span;

#[derive(Debug, Clone)]
pub struct Ty {
    pub id: NodeId,
    pub kind: TyKind,
    pub span: Span,
}

impl Ty {
    pub fn is_const(&self) -> bool {
        match &self.kind {
            TyKind::Const(_) | TyKind::Ref(_, Const::Yes) | TyKind::Ptr(_, Const::Yes) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Yes,
    No,
}

impl Const {
    pub fn from_bool(b: bool) -> Self {
        if b { Const::Yes } else { Const::No }
    }
    pub fn as_bool(&self) -> bool {
        match self {
            Const::Yes => true,
            Const::No => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TyKind {
    /// Reference type eg. `&T` (brim) -> `T&` (C++) or `const &T` (brim) -> `const T&` (C++)
    Ref(Box<Ty>, Const),
    /// Pointer type eg. `*T` (brim) -> `T*` (C++) or `const *T` (brim) -> `const T*` (C++)
    Ptr(Box<Ty>, Const),
    /// Const type eg. `const T` (brim) -> `const T` (C++)
    Const(Box<Ty>),
    /// Array type eg. `[T; N]` (brim) -> `T[N]` (C++)
    Array(Box<Ty>, ConstExpr),
    /// Vector type eg. `T[]` (brim) -> `std::vector<T>` (C++). Resizable array. The syntax in brim is the same as array in C++.
    Vec(Box<Ty>),
    /// Primitive type eg. `i32` (brim) -> `int32_t` (C++)
    Primitive(PrimitiveType),
    /// Any other type that can be enum, struct, type, etc.
    Ident { ident: Ident, generics: Generics },

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

    // Unsigned integers
    U8,
    U16,
    U32,
    U64,

    // Floating point numbers
    F32,
    F64,

    Bool,
    Char,
    Str,
}

impl PrimitiveType {
    pub fn try_from_ident(s: Ident) -> Option<Self> {
        let s = s.name.as_str().unwrap();
        match s.as_str() {
            "i8" => Some(PrimitiveType::I8),
            "i16" => Some(PrimitiveType::I16),
            "i32" => Some(PrimitiveType::I32),
            "i64" => Some(PrimitiveType::I64),

            "u8" => Some(PrimitiveType::U8),
            "u16" => Some(PrimitiveType::U16),
            "u32" => Some(PrimitiveType::U32),
            "u64" => Some(PrimitiveType::U64),

            "f32" => Some(PrimitiveType::F32),
            "f64" => Some(PrimitiveType::F64),

            "bool" => Some(PrimitiveType::Bool),
            "char" => Some(PrimitiveType::Char),
            "str" => Some(PrimitiveType::Str),
            _ => None,
        }
    }
}

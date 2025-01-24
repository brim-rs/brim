use crate::{HirId, expr::HirConstExpr, items::HirGenerics};
use brim_ast::{ErrorEmitted, expr::ConstExpr, item::Ident};
use brim_span::span::Span;

#[derive(Debug, Clone)]
pub struct HirTy {
    pub id: HirId,
    pub kind: HirTyKind,
    pub span: Span,
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
}

#[derive(Debug, Clone)]
pub enum HirTyKind {
    /// Reference type eg. `&T` (brim) -> `T&` (C++) or `const &T` (brim) -> `const T&` (C++)
    Ref(Box<HirTy>, Const),
    /// Pointer type eg. `*T` (brim) -> `T*` (C++) or `const *T` (brim) -> `const T*` (C++)
    Ptr(Box<HirTy>, Const),
    /// Const type eg. `const T` (brim) -> `const T` (C++)
    Const(Box<HirTy>),
    /// Array type eg. `[T; N]` (brim) -> `T[N]` (C++)
    Array(Box<HirTy>, HirConstExpr),
    /// Vector type eg. `T[]` (brim) -> `std::vector<T>` (C++). Resizable array. The syntax in brim is the same as array in C++.
    Vec(Box<HirTy>),
    /// Primitive type eg. `i32` (brim) -> `int32_t` (C++)
    Primitive(PrimitiveType),
    /// Any other type that can be enum, struct, type, etc.
    Ident { ident: Ident, generics: HirGenerics },

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

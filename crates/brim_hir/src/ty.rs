use crate::{HirId, expr::HirConstExpr, items::HirGenerics};
use brim_ast::{
    ErrorEmitted,
    item::Ident,
    ty::{Const, PrimitiveType},
};
use brim_span::span::Span;

#[derive(Debug, Clone)]
pub struct HirTy {
    pub id: HirId,
    pub kind: HirTyKind,
    pub span: Span,
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
    /// Vector type eg. `T[]` (brim) -> `std::vector<T>` (C++). Resizable array. The syntax in brim
    /// is the same as array in C++.
    Vec(Box<HirTy>),
    /// Primitive type eg. `i32` (brim) -> `int32_t` (C++)
    Primitive(PrimitiveType),
    /// Any other type that can be enum, struct, type, etc.
    Ident { ident: Ident, generics: HirGenerics },

    /// Indicating that the compiler failed to determine the type
    Err(ErrorEmitted),

    /// Placeholder for the type of expression that has not been type checked yet
    Placeholder,
}

impl HirTyKind {
    pub fn is_numeric(&self) -> bool {
        match self {
            HirTyKind::Primitive(PrimitiveType::I8)
            | HirTyKind::Primitive(PrimitiveType::I16)
            | HirTyKind::Primitive(PrimitiveType::I32)
            | HirTyKind::Primitive(PrimitiveType::I64)

            | HirTyKind::Primitive(PrimitiveType::U8)
            | HirTyKind::Primitive(PrimitiveType::U16)
            | HirTyKind::Primitive(PrimitiveType::U32)
            | HirTyKind::Primitive(PrimitiveType::U64)

            | HirTyKind::Primitive(PrimitiveType::F32)
            | HirTyKind::Primitive(PrimitiveType::F64) => true,
            _ => false,
        }
    }
    
    pub fn is_bool(&self) -> bool {
        match self {
            HirTyKind::Primitive(PrimitiveType::Bool) => true,
            _ => false,
        }
    }
    
    pub fn can_be_dereferenced(&self) -> bool {
        match self {
            HirTyKind::Ref(_, _) | HirTyKind::Ptr(_, _) => true,
            _ => false,
        }
    }
    
    pub fn err() -> Self {
        HirTyKind::Err(ErrorEmitted::new())
    }
    
    pub fn void() -> Self {
        HirTyKind::Primitive(PrimitiveType::Void)
    }
}
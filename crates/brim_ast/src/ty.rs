use crate::{ErrorEmitted, NodeId, expr::ConstExpr};
use brim_span::span::Span;

#[derive(Debug, Clone)]
pub struct Ty {
    pub id: NodeId,
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Yes,
    No,
}

impl Const {
    pub fn from_bool(b: bool) -> Self {
        if b {
            Const::Yes
        } else {
            Const::No
        }
    }
}

#[derive(Debug, Clone)]
pub enum TyKind {
    /// Reference type eg. `&T` (brim) -> `T&` (C++) or `const &T` (brim) -> `const T&` (C++)
    Ref(Box<Ty>, Const),
    /// Pointer type eg. `*T` (brim) -> `T*` (C++) or `const *T` (brim) -> `const T*` (C++)
    Ptr(Box<Ty>, Const),
    /// Array type eg. `[T; N]` (brim) -> `T[N]` (C++)
    Array(Box<Ty>, ConstExpr),
    /// Vector type eg. `T[]` (brim) -> `std::vector<T>` (C++). Resizable array. The syntax in brim is the same as array in C++.
    Vec(Box<Ty>),
    /// Primitive type eg. `i32` (brim) -> `int32_t` (C++)
    Primitive(PrimitiveType),

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
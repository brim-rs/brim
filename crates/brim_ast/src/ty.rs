use crate::{ErrorEmitted, NodeId, expr::ConstExpr};
use brim_span::span::Span;

#[derive(Debug, Clone)]
pub struct Ty {
    pub id: NodeId,
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Const {
    Yes,
    No,
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

    /// Indicating that the compiler failed to determine the type
    Err(ErrorEmitted),
}

use crate::{HirId, expr::HirConstExpr, items::HirGenericArgs};
use brim_ast::{
    item::Ident,
    ty::{Const, PrimitiveType},
};
use brim_diagnostics::{ErrorEmitted, diagnostic::ToDiagnostic};
use brim_span::span::Span;
use std::fmt::{Debug, Display};
use brim_diagnostics::diagnostic::Diagnostic;

#[derive(Debug, Clone)]
pub struct HirTy {
    pub id: HirId,
    pub kind: HirTyKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirTyKind {
    /// Reference type eg. `&T` (brim) -> `T&` (C++) or `const &T` (brim) -> `const T&` (C++)
    Ref(Box<HirTyKind>, Const),
    /// Pointer type eg. `*T` (brim) -> `T*` (C++) or `const *T` (brim) -> `const T*` (C++)
    Ptr(Box<HirTyKind>, Const),
    /// Const type eg. `const T` (brim) -> `const T` (C++)
    Const(Box<HirTyKind>),
    /// Array type eg. `[T; N]` (brim) -> `T[N]` (C++)
    Array(Box<HirTyKind>, HirConstExpr),
    /// Vector type eg. `T[]` (brim) -> `std::vector<T>` (C++). Resizable array. The syntax in brim
    /// is the same as array in C++.
    Vec(Box<HirTyKind>),
    /// Primitive type eg. `i32` (brim) -> `int32_t` (C++)
    Primitive(PrimitiveType),
    /// Any other type that can be enum, struct, type, etc.
    Ident {
        ident: Ident,
        generics: HirGenericArgs,
    },
    /// Result type eg. `T!E` (brim) -> `std::expected<T, E>` (C++).
    Result(Box<HirTyKind>, Box<HirTyKind>),

    /// Indicating that the compiler failed to determine the type
    Err(Diagnostic<usize>),

    /// Placeholder for the type of expression that has not been type checked yet
    Placeholder,
}

impl Display for HirTyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HirTyKind::Ref(ty, _) => write!(f, "&{}", ty),
            HirTyKind::Ptr(ty, _) => write!(f, "*{}", ty),
            HirTyKind::Const(ty) => write!(f, "const {}", ty),
            HirTyKind::Array(ty, len) => write!(f, "[{}; {:?}]", ty, len),
            HirTyKind::Vec(ty) => write!(f, "{}[]", ty),
            HirTyKind::Primitive(p) => write!(f, "{:?}", p),
            HirTyKind::Ident { ident, generics } => {
                write!(f, "{}{}", ident, generics)
            }
            HirTyKind::Result(ok, err) => write!(f, "{}!{}", ok, err),
            HirTyKind::Err(diag) => write!(f, ""),
            HirTyKind::Placeholder => write!(f, "_"),
        }
    }
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

    pub fn err(diag: impl ToDiagnostic + 'static) -> Self {
        HirTyKind::Err(diag.to_diagnostic())
    }

    pub fn void() -> Self {
        HirTyKind::Primitive(PrimitiveType::Void)
    }

    pub fn to_primitive(&self) -> PrimitiveType {
        match self {
            HirTyKind::Primitive(p) => p.clone(),
            _ => panic!("Expected primitive type, found {:?}", self),
        }
    }

    pub fn can_be_logically_compared_to(&self, other: &HirTyKind) -> bool {
        match (self, other) {
            (HirTyKind::Primitive(_), HirTyKind::Primitive(_)) => true,
            // TODO: depends if the length changes the type
            (HirTyKind::Array(ty1, _), HirTyKind::Array(ty2, _)) => {
                ty1.can_be_logically_compared_to(&ty2)
            }
            (HirTyKind::Const(ty1), HirTyKind::Const(ty2)) => {
                ty1.can_be_logically_compared_to(&ty2)
            }
            (HirTyKind::Vec(ty1), HirTyKind::Vec(ty2)) => ty1.can_be_logically_compared_to(&ty2),
            _ => false,
        }
    }
}

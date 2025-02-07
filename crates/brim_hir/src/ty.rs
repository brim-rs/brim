use crate::{HirId, expr::HirConstExpr, items::HirGenericArgs};
use brim_ast::{
    item::Ident,
    ty::{Const, PrimitiveType},
};
use brim_diagnostics::{
    ErrorEmitted,
    diagnostic::{Diagnostic, ToDiagnostic},
};
use brim_span::span::Span;
use std::fmt::{Debug, Display};
use brim_ast::token::{Lit, LitKind};

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
    Array(Box<HirTyKind>, usize),
    /// Vector type eg. `T[]` (brim) -> `std::vector<T>` (C++). Resizable array. The syntax in brim
    /// is the same as array in C++.
    Vec(Box<HirTyKind>),
    /// Primitive type eg. `i32` (brim) -> `int32_t` (C++)
    Primitive(PrimitiveType),
    /// Any other type that can be enum, struct, type, etc.
    Ident {
        ident: Ident,
        generics: HirGenericArgs,
        is_generic: bool,
    },
    /// Result type eg. `T!E` (brim) -> `std::expected<T, E>` (C++).
    Result(Box<HirTyKind>, Box<HirTyKind>),
    /// Ok type eg. `@ok` (brim) -> `std::expected<T, E>` (C++)
    ResOk(Box<HirTyKind>),
    /// Error type eg. `@err` (brim) -> `std::unexpected<T, E>` (C++)
    ResErr(Box<HirTyKind>),

    /// Indicating that the compiler failed to determine the type
    Err(ErrorEmitted),

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
            HirTyKind::Primitive(p) => write!(f, "{}", p),
            HirTyKind::Ident { ident, generics, .. } => {
                write!(f, "{}{}", ident, if generics.params.len() > 0 {
                    generics.to_string()
                } else {
                    "".to_string()
                })
            }
            HirTyKind::Result(ok, err) => write!(f, "{}!{}", ok, err),
            HirTyKind::ResOk(ty) => write!(f, "@ok{}", ty),
            HirTyKind::ResErr(ty) => write!(f, "@err{}", ty),
            HirTyKind::Err(diag) => write!(f, ""),
            HirTyKind::Placeholder => write!(f, "_"),
        }
    }
}

impl PartialEq for HirTyKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (HirTyKind::Ref(ty1, c1), HirTyKind::Ref(ty2, c2)) => ty1 == ty2 && c1 == c2,
            (HirTyKind::Ptr(ty1, c1), HirTyKind::Ptr(ty2, c2)) => ty1 == ty2 && c1 == c2,
            (HirTyKind::Const(ty1), HirTyKind::Const(ty2)) => ty1 == ty2,
            (HirTyKind::Array(ty1, len1), HirTyKind::Array(ty2, len2)) => {
                ty1 == ty2 && len1 == len2
            }
            (HirTyKind::Vec(ty1), HirTyKind::Vec(ty2)) => ty1 == ty2,
            (HirTyKind::Primitive(p1), HirTyKind::Primitive(p2)) => p1 == p2,
            (
                // TODO: compare modules
                HirTyKind::Ident {
                    ident: id1,
                    generics: gen1,
                    is_generic: is_generic1
                },
                HirTyKind::Ident {
                    ident: id2,
                    generics: gen2,
                    is_generic: is_generic2
                },
            ) => id1 == id2 && gen1 == gen2 && is_generic1 == is_generic2,
            (HirTyKind::Result(ok1, err1), HirTyKind::Result(ok2, err2)) => {
                ok1 == ok2 && err1 == err2
            }
            (HirTyKind::Err(diag1), HirTyKind::Err(diag2)) => diag1 == diag2,
            (HirTyKind::Placeholder, HirTyKind::Placeholder) => true,
            _ => false,
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

    pub fn err() -> Self {
        HirTyKind::Err(ErrorEmitted::new())
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
    
    pub fn from_lit(lit: &LitKind) -> Self {
        match lit {
            LitKind::Integer => HirTyKind::Primitive(PrimitiveType::I32),
            LitKind::Float => HirTyKind::Primitive(PrimitiveType::F32),
            LitKind::Bool => HirTyKind::Primitive(PrimitiveType::Bool),
            LitKind::Char => HirTyKind::Primitive(PrimitiveType::Char),
            LitKind::Str => HirTyKind::Primitive(PrimitiveType::Str),
            LitKind::ByteStr => HirTyKind::Vec(Box::new(HirTyKind::Primitive(PrimitiveType::U8))),
            LitKind::Byte => HirTyKind::Primitive(PrimitiveType::U8),
            _ => unimplemented!(),
        }
    }
    
    pub fn is_result(&self) -> Option<(Box<HirTyKind>, Box<HirTyKind>)> {
        match self {
            HirTyKind::Result(ok, err) => Some((ok.clone(), err.clone())),
            _ => None,
        }
    }
}

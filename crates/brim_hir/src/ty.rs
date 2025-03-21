use crate::items::HirGenericArgs;
use brim_ast::{
    ItemId,
    item::Ident,
    token::LitKind,
    ty::{Mutable, PrimitiveType},
};
use brim_diagnostics::ErrorEmitted;
use brim_span::span::Span;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone)]
pub struct HirTy {
    pub id: ItemId,
    pub kind: HirTyKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirTyKind {
    /// Reference type eg. `&T` (brim) -> `T&` (C++) or `&const T` (brim) -> `const T&` (C++)
    Ref(Box<HirTyKind>, Mutable),
    /// Pointer type eg. `*T` (brim) -> `T*` (C++) or `*const T` (brim) -> `const T*` (C++)
    Ptr(Box<HirTyKind>, Mutable),
    /// Mut type eg. `mut T` (brim) -> `T` (C++)
    Mut(Box<HirTyKind>),
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

    /// Indicating that the compiler failed to determine the type
    Err(ErrorEmitted),

    /// Placeholder for the type of expression that has not been type checked yet
    Placeholder,
}

impl Display for HirTyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HirTyKind::Ref(ty, mutable) => {
                let const_str = if *mutable == Mutable::Yes { "mut " } else { "" };
                write!(f, "&{}{}", const_str, ty)
            }
            HirTyKind::Ptr(ty, mutable) => {
                let const_str = if *mutable == Mutable::Yes { "mut " } else { "" };
                write!(f, "*{}{}", const_str, ty)
            }
            HirTyKind::Mut(ty) => write!(f, "mut {}", ty),
            HirTyKind::Array(ty, len) => write!(f, "[{}; {}]", ty, len),
            HirTyKind::Vec(ty) => write!(f, "{}[]", ty),
            HirTyKind::Primitive(p) => write!(f, "{}", p),
            HirTyKind::Ident {
                ident, generics, ..
            } => {
                if generics.params.is_empty() {
                    write!(f, "{}", ident)
                } else {
                    write!(f, "{}{}", ident, generics)
                }
            }
            HirTyKind::Err(_) => write!(f, "<error>"),
            HirTyKind::Placeholder => write!(f, "_"),
        }
    }
}

impl PartialEq for HirTyKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (HirTyKind::Primitive(PrimitiveType::Any), _) => true,
            (HirTyKind::Ref(ty1, c1), HirTyKind::Ref(ty2, c2)) => ty1 == ty2,
            (HirTyKind::Ptr(ty1, c1), HirTyKind::Ptr(ty2, c2)) => ty1 == ty2,
            (HirTyKind::Ref(ty1, _) | HirTyKind::Ptr(ty1, _), ty2) => *ty1 == Box::new(ty2.clone()),
            (HirTyKind::Mut(ty1), HirTyKind::Mut(ty2)) => ty1 == ty2,
            (HirTyKind::Mut(ty1), ty2) => *ty1 == Box::new(ty2.clone()),
            (HirTyKind::Array(ty1, len1), HirTyKind::Array(ty2, len2)) => {
                ty1 == ty2 && len1 == len2
            }
            (HirTyKind::Vec(ty1), HirTyKind::Vec(ty2)) => ty1 == ty2,
            (HirTyKind::Primitive(p1), HirTyKind::Primitive(p2)) => p1 == p2,
            (
                HirTyKind::Ident {
                    ident: id1,
                    generics: gen1,
                    is_generic: is_generic1,
                },
                HirTyKind::Ident {
                    ident: id2,
                    generics: gen2,
                    is_generic: is_generic2,
                },
            ) => id1.to_string() == id2.to_string() && gen1 == gen2 && is_generic1 == is_generic2,
            (HirTyKind::Err(diag1), HirTyKind::Err(diag2)) => diag1 == diag2,
            (HirTyKind::Placeholder, HirTyKind::Placeholder) => true,
            _ => false,
        }
    }
}

impl Eq for HirTyKind {}

impl HirTyKind {
    pub fn can_be_an_arg_for_param(&self, param: &HirTyKind) -> bool {
        match (param, self) {
            (HirTyKind::Primitive(PrimitiveType::Any), _) => true,

            (
                HirTyKind::Mut(ty1)
                | HirTyKind::Ref(ty1, Mutable::Yes)
                | HirTyKind::Ptr(ty1, Mutable::Yes),
                HirTyKind::Mut(ty2)
                | HirTyKind::Ref(ty2, Mutable::Yes)
                | HirTyKind::Ptr(ty2, Mutable::Yes),
            ) => ty1 == ty2,

            (
                HirTyKind::Mut(ty1)
                | HirTyKind::Ref(ty1, Mutable::Yes)
                | HirTyKind::Ptr(ty1, Mutable::Yes),
                _,
            ) => false,

            (HirTyKind::Array(ty1, len1), HirTyKind::Array(ty2, len2)) => {
                ty1 == ty2 && len1 == len2
            }

            (HirTyKind::Vec(ty1), HirTyKind::Vec(ty2)) => ty1 == ty2,

            (HirTyKind::Primitive(p1), HirTyKind::Primitive(p2)) => p1 == p2,

            (
                HirTyKind::Ident {
                    ident: id1,
                    generics: gen1,
                    is_generic: is_gen1,
                },
                HirTyKind::Ident {
                    ident: id2,
                    generics: gen2,
                    is_generic: is_gen2,
                },
            ) => id1.to_string() == id2.to_string() && gen1 == gen2 && is_gen1 == is_gen2,

            (HirTyKind::Err(_), _) | (_, HirTyKind::Err(_)) => false,
            (HirTyKind::Placeholder, _) | (_, HirTyKind::Placeholder) => false,

            _ => false,
        }
    }

    pub fn can_be_initialized_with(&self, other: &HirTyKind) -> bool {
        match (self, other) {
            (HirTyKind::Primitive(PrimitiveType::Any), _) => true,
            (HirTyKind::Ref(ty1, _), ty2) => ty1.can_be_initialized_with(ty2),
            (HirTyKind::Ptr(ty1, _), ty2) => ty1.can_be_initialized_with(ty2),
            (HirTyKind::Mut(ty1), ty2) => ty1.as_ref().can_be_initialized_with(ty2),
            (ty1, HirTyKind::Mut(ty2)) => ty1.can_be_initialized_with(ty2.as_ref()),
            (HirTyKind::Array(ty1, _), HirTyKind::Array(ty2, _)) => {
                ty1.can_be_initialized_with(ty2)
            }
            (HirTyKind::Vec(ty1), HirTyKind::Vec(ty2)) => ty1.can_be_initialized_with(ty2),
            (HirTyKind::Primitive(p1), HirTyKind::Primitive(p2)) => p1 == p2,
            (
                HirTyKind::Ident {
                    ident: id1,
                    generics: gen1,
                    is_generic: is_gen1,
                },
                HirTyKind::Ident {
                    ident: id2,
                    generics: gen2,
                    is_generic: is_gen2,
                },
            ) => id1.to_string() == id2.to_string() && gen1 == gen2 && is_gen1 == is_gen2,

            (HirTyKind::Err(_), _) | (_, HirTyKind::Err(_)) => false,
            (HirTyKind::Placeholder, _) | (_, HirTyKind::Placeholder) => false,

            _ => false,
        }
    }

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
            HirTyKind::Mut(ty) => ty.is_numeric(),
            HirTyKind::Ref(ty, _) => ty.is_numeric(),
            HirTyKind::Ptr(ty, _) => ty.is_numeric(),
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
            HirTyKind::Mut(ty) => ty.to_primitive(),
            HirTyKind::Ref(ty, _) => ty.to_primitive(),
            HirTyKind::Ptr(ty, _) => ty.to_primitive(),
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
            (HirTyKind::Mut(ty1), HirTyKind::Mut(ty2)) => ty1.can_be_logically_compared_to(&ty2),
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

    pub fn as_ident(&self) -> Option<(Ident, HirGenericArgs)> {
        match self {
            HirTyKind::Ident {
                ident,
                generics,
                is_generic,
            } => Some((ident.clone(), generics.clone())),
            _ => None,
        }
    }

    /// Basically if type from type declaration can be directly used as an inferred type
    pub fn can_be_directly_used(&self) -> bool {
        match self {
            HirTyKind::Primitive(_)
            | HirTyKind::Array(_, _)
            | HirTyKind::Ptr(_, _)
            | HirTyKind::Ref(_, _) => true,
            _ => false,
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            HirTyKind::Ref(_, Mutable::Yes)
            | HirTyKind::Ptr(_, Mutable::Yes)
            | HirTyKind::Mut(_) => true,
            _ => false,
        }
    }
}

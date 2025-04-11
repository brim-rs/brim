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

#[derive(Debug, Clone, PartialEq)]
pub enum HirTyKind {
    /// Reference type eg. `&T` (brim) -> `T&` (C++) or `&const T` (brim) -> `const T&` (C++)
    Ref(Box<HirTyKind>, Mutable),
    /// Pointer type eg. `*T` (brim) -> `T*` (C++) or `*const T` (brim) -> `const T*` (C++)
    Ptr(Box<HirTyKind>, Mutable),
    /// Mut type eg. `mut T` (brim) -> `T` (C++)
    Mut(Box<HirTyKind>),
    /// Const type eg. `const T` (brim) -> `const T` (C++)
    Const(Box<HirTyKind>),
    /// Vector type eg. `T[]` (brim) -> `std::vector<T>` (C++). Resizable array. The syntax in brim
    /// is the same as array in C++.
    Vec(Box<HirTyKind>),
    /// Primitive type eg. `i32` (brim) -> `int32_t` (C++)
    Primitive(PrimitiveType),
    /// Any other type that can be enum, struct, type, etc.
    Ident { ident: Ident, generics: HirGenericArgs, is_generic: bool },

    /// Result type eg. `Result<T, E>` (brim) -> `std::expected<T, E>` (C++)
    Result(Box<HirTyKind>, Box<HirTyKind>),
    /// Ok variant of Result type.
    ResultOk(Box<HirTyKind>),
    /// Err variant of Result type.
    ResultErr(Box<HirTyKind>),

    /// Optional type
    Option(Box<HirTyKind>),
    /// No value
    None,
    /// Some value
    Some(Box<HirTyKind>),

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
                write!(f, "&{const_str}{ty}")
            }
            HirTyKind::Ptr(ty, mutable) => {
                let const_str = if *mutable == Mutable::Yes { "mut " } else { "" };
                write!(f, "*{const_str}{ty}")
            }
            HirTyKind::Mut(ty) => write!(f, "mut {ty}"),
            HirTyKind::Const(ty) => write!(f, "const {ty}"),
            HirTyKind::Vec(ty) => write!(f, "{ty}[]"),
            HirTyKind::Primitive(p) => write!(f, "{p}"),
            HirTyKind::Ident { ident, generics, .. } => {
                if generics.params.is_empty() {
                    write!(f, "{ident}")
                } else {
                    write!(f, "{ident}{generics}")
                }
            }

            HirTyKind::Result(ty, err) => write!(f, "{ty}!{err}"),
            HirTyKind::ResultOk(ty) => write!(f, "@ok({ty})"),
            HirTyKind::ResultErr(err) => write!(f, "@err({err})"),

            HirTyKind::Option(ty) => write!(f, "{ty}?"),
            HirTyKind::None => write!(f, "None"),
            HirTyKind::Some(ty) => write!(f, "Some({ty})"),

            HirTyKind::Err(_) => write!(f, "<error>"),
            HirTyKind::Placeholder => write!(f, "_"),
        }
    }
}

impl HirTyKind {
    pub fn simple_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (HirTyKind::Primitive(PrimitiveType::Any), _) => true,

            (HirTyKind::Ref(ty1, Mutable::No), HirTyKind::Ref(ty2, Mutable::Yes)) => {
                ty1.simple_eq(ty2)
            }

            (HirTyKind::Ref(ty1, mutability1), HirTyKind::Ref(ty2, mutability2)) => {
                ty1.simple_eq(ty2) && mutability1 == mutability2
            }

            (HirTyKind::Ref(_, _), _) => false,
            (_, HirTyKind::Ref(_, _)) => false,

            (HirTyKind::Ptr(ty1, Mutable::No), HirTyKind::Ptr(ty2, Mutable::Yes)) => {
                ty1.simple_eq(ty2)
            }

            (HirTyKind::Ptr(ty1, mutability1), HirTyKind::Ptr(ty2, mutability2)) => {
                ty1.simple_eq(ty2) && mutability1 == mutability2
            }

            (HirTyKind::Ptr(_, _), _) => false,
            (_, HirTyKind::Ptr(_, _)) => false,

            (HirTyKind::Mut(ty1), HirTyKind::Mut(ty2)) => ty1.simple_eq(ty2),
            (HirTyKind::Mut(ty1), ty2) => ty1.simple_eq(ty2),
            (ty1, HirTyKind::Mut(ty2)) => ty1.simple_eq(ty2),

            (HirTyKind::Vec(ty1), HirTyKind::Vec(ty2)) => ty1.simple_eq(ty2),

            (HirTyKind::Primitive(p1), HirTyKind::Primitive(p2)) => p1 == p2,

            (HirTyKind::Result(ok1, err1), HirTyKind::Result(ok2, err2)) => {
                ok1.simple_eq(ok2) && err1.simple_eq(err2)
            }
            (HirTyKind::ResultOk(ok1), HirTyKind::ResultOk(ok2)) => ok1.simple_eq(ok2),
            (HirTyKind::ResultErr(err1), HirTyKind::ResultErr(err2)) => err1.simple_eq(err2),
            (HirTyKind::ResultOk(_), HirTyKind::ResultErr(_)) => false,
            (HirTyKind::ResultErr(_), HirTyKind::ResultOk(_)) => false,
            (HirTyKind::Result(ok1, _), HirTyKind::ResultOk(ok2)) => ok1.simple_eq(ok2),
            (HirTyKind::Result(_, err1), HirTyKind::ResultErr(err2)) => err1.simple_eq(err2),
            (HirTyKind::ResultOk(ok1), HirTyKind::Result(ok2, _)) => ok1.simple_eq(ok2),
            (HirTyKind::ResultErr(err1), HirTyKind::Result(_, err2)) => err1.simple_eq(err2),

            (HirTyKind::Option(ty1), HirTyKind::Option(ty2)) => ty1.simple_eq(ty2),
            (HirTyKind::Some(val1), HirTyKind::Some(val2)) => val1.simple_eq(val2),
            (HirTyKind::None, HirTyKind::None) => true,
            (HirTyKind::Some(_), HirTyKind::None) => true,
            (HirTyKind::None, HirTyKind::Some(_)) => true,
            (HirTyKind::Option(val1), HirTyKind::Some(val2)) => val1.simple_eq(val2),
            (HirTyKind::Some(val1), HirTyKind::Option(val2)) => val1.simple_eq(val2),
            (HirTyKind::Option(_), HirTyKind::None) => true,
            (HirTyKind::None, HirTyKind::Option(_)) => true,

            (
                HirTyKind::Ident { ident: id1, generics: gen1, is_generic: g1 },
                HirTyKind::Ident { ident: id2, generics: gen2, is_generic: g2 },
            ) => id1.to_string() == id2.to_string() && gen1 == gen2 && g1 == g2,

            (HirTyKind::Err(diag1), HirTyKind::Err(diag2)) => diag1 == diag2,
            (HirTyKind::Placeholder, HirTyKind::Placeholder) => true,

            _ => false,
        }
    }

    pub fn can_be_an_arg_for_param(&self, param: &HirTyKind) -> bool {
        match (param, self) {
            (HirTyKind::Primitive(PrimitiveType::Any), _) => true,

            (HirTyKind::Ptr(param_ty, mp), HirTyKind::Ref(arg_ty, ma)) => {
                param_ty.can_be_an_arg_for_param(arg_ty) && *mp == *ma
            }
            (HirTyKind::Ptr(param_ty, mp), HirTyKind::Ptr(arg_ty, ma)) => {
                param_ty.can_be_an_arg_for_param(arg_ty) && *mp == *ma
            }
            (HirTyKind::Const(ty1), HirTyKind::Const(ty2)) => ty1.can_be_an_arg_for_param(ty2),
            (HirTyKind::Const(ty1), ty2) => ty1.can_be_an_arg_for_param(ty2),
            (ty1, HirTyKind::Const(ty2)) => ty1.can_be_an_arg_for_param(ty2),

            (HirTyKind::Mut(ty1), HirTyKind::Mut(ty2)) => ty1.can_be_an_arg_for_param(ty2),

            (HirTyKind::Vec(ty1), HirTyKind::Vec(ty2)) => ty1.can_be_an_arg_for_param(ty2),

            (HirTyKind::Primitive(p1), HirTyKind::Primitive(p2)) => p1 == p2,

            (HirTyKind::Result(ok1, err1), HirTyKind::Result(ok2, err2)) => {
                ok1.can_be_an_arg_for_param(ok2) && err1.can_be_an_arg_for_param(err2)
            }
            (HirTyKind::ResultOk(ty1), HirTyKind::ResultOk(ty2)) => {
                ty1.can_be_an_arg_for_param(ty2)
            }
            (HirTyKind::ResultErr(ty1), HirTyKind::ResultErr(ty2)) => {
                ty1.can_be_an_arg_for_param(ty2)
            }
            (HirTyKind::Result(ok1, _), HirTyKind::ResultOk(ok2)) => {
                ok1.can_be_an_arg_for_param(ok2)
            }
            (HirTyKind::Result(_, err1), HirTyKind::ResultErr(err2)) => {
                err1.can_be_an_arg_for_param(err2)
            }
            (HirTyKind::ResultOk(ok1), HirTyKind::Result(ok2, _)) => {
                ok1.can_be_an_arg_for_param(ok2)
            }
            (HirTyKind::ResultErr(err1), HirTyKind::Result(_, err2)) => {
                err1.can_be_an_arg_for_param(err2)
            }

            (HirTyKind::Option(ty1), HirTyKind::Option(ty2)) => ty1.can_be_an_arg_for_param(ty2),
            (HirTyKind::Some(val1), HirTyKind::Some(val2)) => val1.can_be_an_arg_for_param(val2),
            (HirTyKind::Option(val1), HirTyKind::Some(val2)) => val2.can_be_an_arg_for_param(val1),
            (HirTyKind::None, HirTyKind::None) => true,
            (HirTyKind::Some(_), HirTyKind::None) => false,
            (HirTyKind::None, HirTyKind::Some(_)) => false,
            (HirTyKind::Some(val1), HirTyKind::Option(val2)) => val1.can_be_an_arg_for_param(val2),
            (HirTyKind::Option(_), HirTyKind::None) => true,
            (HirTyKind::None, HirTyKind::Option(_)) => true,

            (
                HirTyKind::Ident { ident: id1, generics: gen1, is_generic: g1 },
                HirTyKind::Ident { ident: id2, generics: gen2, is_generic: g2 },
            ) => id1.to_string() == id2.to_string() && gen1 == gen2 && g1 == g2,

            (HirTyKind::Err(_), _) | (_, HirTyKind::Err(_)) => false,
            (HirTyKind::Placeholder, _) | (_, HirTyKind::Placeholder) => false,

            _ => false,
        }
    }

    pub fn can_be_initialized_with(&self, other: &HirTyKind) -> bool {
        match (self, other) {
            (HirTyKind::Primitive(PrimitiveType::Any), _) => true,

            (HirTyKind::Ptr(ty1, _), HirTyKind::Ref(ty2, _)) => ty1.simple_eq(ty2),

            (HirTyKind::Ptr(ty1, m), HirTyKind::Ptr(ty2, m1)) => {
                ty1.simple_eq(ty2) && (*m == *m1 || *m == Mutable::No)
            }
            (HirTyKind::Ptr(_, Mutable::Yes), _) => false,

            (HirTyKind::Mut(ty1), ty2) => ty1.can_be_initialized_with(ty2),
            (ty1, HirTyKind::Mut(ty2)) => ty1.can_be_initialized_with(ty2),

            (HirTyKind::Vec(ty1), HirTyKind::Vec(ty2)) => ty1.can_be_initialized_with(ty2),

            (HirTyKind::Primitive(p1), HirTyKind::Primitive(p2)) => p1 == p2,

            (HirTyKind::Result(ok1, err1), HirTyKind::Result(ok2, err2)) => {
                ok1.can_be_initialized_with(ok2) && err1.can_be_initialized_with(err2)
            }
            (HirTyKind::Option(ty1), HirTyKind::Option(ty2)) => ty1.can_be_initialized_with(ty2),
            (HirTyKind::Some(val1), HirTyKind::Some(val2)) => val1.can_be_initialized_with(val2),
            (HirTyKind::None, HirTyKind::None) => true,
            (HirTyKind::Option(val1), HirTyKind::Some(val2)) => val1.can_be_initialized_with(val2),
            (HirTyKind::Some(val1), HirTyKind::Option(val2)) => val1.can_be_initialized_with(val2),
            (HirTyKind::Option(_), HirTyKind::None) => true,
            (HirTyKind::None, HirTyKind::Option(_)) => true,
            (HirTyKind::Some(_), HirTyKind::None) => false,
            (HirTyKind::None, HirTyKind::Some(_)) => false,

            (
                HirTyKind::Ident { ident: id1, generics: gen1, is_generic: g1 },
                HirTyKind::Ident { ident: id2, generics: gen2, is_generic: g2 },
            ) => id1.to_string() == id2.to_string() && gen1 == gen2 && g1 == g2,

            (HirTyKind::Err(_), _) | (_, HirTyKind::Err(_)) => false,
            (HirTyKind::Placeholder, _) | (_, HirTyKind::Placeholder) => false,

            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            HirTyKind::Primitive(
                PrimitiveType::I8
                | PrimitiveType::I16
                | PrimitiveType::I32
                | PrimitiveType::I64
                | PrimitiveType::U8
                | PrimitiveType::U16
                | PrimitiveType::U32
                | PrimitiveType::U64
                | PrimitiveType::F32
                | PrimitiveType::F64,
            ) => true,
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
            _ => panic!("Expected primitive type, found {self:?}"),
        }
    }

    pub fn can_be_logically_compared_to(&self, other: &HirTyKind) -> bool {
        match (self, other) {
            (HirTyKind::Primitive(_), HirTyKind::Primitive(_)) => true,
            (HirTyKind::Mut(ty1), HirTyKind::Mut(ty2)) => ty1.can_be_logically_compared_to(ty2),
            (HirTyKind::Vec(ty1), HirTyKind::Vec(ty2)) => ty1.can_be_logically_compared_to(ty2),
            _ => false,
        }
    }

    pub fn from_lit(lit: &LitKind) -> Self {
        match lit {
            LitKind::Integer => HirTyKind::Primitive(PrimitiveType::I32),
            LitKind::Float => HirTyKind::Primitive(PrimitiveType::F32),
            LitKind::Bool => HirTyKind::Primitive(PrimitiveType::Bool),
            LitKind::Char => HirTyKind::Primitive(PrimitiveType::Char),
            LitKind::Str => HirTyKind::Primitive(PrimitiveType::String),
            LitKind::ByteStr => HirTyKind::Vec(Box::new(HirTyKind::Primitive(PrimitiveType::U8))),
            LitKind::Byte => HirTyKind::Primitive(PrimitiveType::U8),
            _ => unimplemented!(),
        }
    }

    pub fn as_ident(&self) -> Option<(Ident, HirGenericArgs)> {
        match self {
            HirTyKind::Ident { ident, generics, .. } => Some((*ident, generics.clone())),
            _ => None,
        }
    }

    /// Basically if type from type declaration can be directly used as an inferred type
    pub fn can_be_directly_used(&self) -> bool {
        match self {
            HirTyKind::Primitive(_) | HirTyKind::Ptr(_, _) | HirTyKind::Ref(_, _) => true,
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

    pub fn can_be_ignored(&self) -> bool {
        match self {
            HirTyKind::Primitive(PrimitiveType::Void) => true,
            _ => false,
        }
    }

    pub fn is_placeholder(&self) -> bool {
        match self {
            HirTyKind::Placeholder => true,
            HirTyKind::ResultOk(ty) | HirTyKind::ResultErr(ty) | HirTyKind::Some(ty) => {
                ty.is_placeholder()
            }
            _ => false,
        }
    }

    pub fn is_option(&self) -> Option<HirTyKind> {
        match self {
            HirTyKind::Option(ty) | HirTyKind::Const(ty) => Some(*ty.clone()),
            _ => None,
        }
    }

    pub fn is_ident(&self) -> Option<Ident> {
        match self {
            HirTyKind::Ident { ident, .. } => Some(*ident),
            HirTyKind::Ptr(ty, _) | HirTyKind::Ref(ty, _) | HirTyKind::Mut(ty) => ty.is_ident(),
            _ => None,
        }
    }

    pub fn is_ref(&self) -> bool {
        match self {
            HirTyKind::Ref(_, _) => true,
            _ => false,
        }
    }

    pub fn is_vector(&self) -> Option<HirTyKind> {
        match self {
            HirTyKind::Vec(ty) => Some(*ty.clone()),
            HirTyKind::Ptr(ty, _)
            | HirTyKind::Ref(ty, _)
            | HirTyKind::Const(ty)
            | HirTyKind::Mut(ty) => ty.is_vector(),
            _ => None,
        }
    }

    pub fn is_const_vector(&self) -> bool {
        match self {
            HirTyKind::Ptr(ty, mutable) | HirTyKind::Ref(ty, mutable) => {
                ty.is_vector().is_some() && mutable == &Mutable::Yes
            }
            HirTyKind::Const(ty) => ty.is_vector().is_some(),
            _ => false,
        }
    }

    pub fn try_promote_type(
        source_ty: &mut HirTyKind,
        target_ty: &HirTyKind,
        is_let_stmt: bool,
    ) -> bool {
        if *source_ty == *target_ty {
            return true;
        }

        match (source_ty.clone(), target_ty) {
            (HirTyKind::Primitive(source_prim), HirTyKind::Primitive(target_prim)) => {
                if is_let_stmt && PrimitiveType::can_initialize_with_type(&source_prim, target_prim)
                {
                    *source_ty = HirTyKind::Primitive(target_prim.clone());
                    return true;
                } else if let Some(promoted) =
                    PrimitiveType::promote_type(&source_prim, target_prim)
                {
                    *source_ty = HirTyKind::Primitive(promoted);
                    return true;
                }
            }

            (
                HirTyKind::Ref(source_inner, source_mut),
                HirTyKind::Ref(target_inner, target_mut),
            ) => {
                if source_mut == *target_mut {
                    let mut_clone = source_mut; // Clone the mutability
                    return HirTyKind::try_promote_wrapped_type(
                        source_ty,
                        &source_inner,
                        target_inner,
                        is_let_stmt,
                        move |inner| HirTyKind::Ref(Box::new(inner), mut_clone),
                    );
                }
            }

            (
                HirTyKind::Ptr(source_inner, source_mut),
                HirTyKind::Ptr(target_inner, target_mut),
            ) => {
                if source_mut == *target_mut {
                    let mut_clone = source_mut; // Clone the mutability
                    return HirTyKind::try_promote_wrapped_type(
                        source_ty,
                        &source_inner,
                        target_inner,
                        is_let_stmt,
                        move |inner| HirTyKind::Ptr(Box::new(inner), mut_clone),
                    );
                }
            }

            (HirTyKind::Mut(source_inner), HirTyKind::Mut(target_inner)) => {
                return HirTyKind::try_promote_wrapped_type(
                    source_ty,
                    &source_inner,
                    target_inner,
                    is_let_stmt,
                    |inner| HirTyKind::Mut(Box::new(inner)),
                );
            }

            (HirTyKind::Const(source_inner), HirTyKind::Const(target_inner)) => {
                return HirTyKind::try_promote_wrapped_type(
                    source_ty,
                    &source_inner,
                    target_inner,
                    is_let_stmt,
                    |inner| HirTyKind::Const(Box::new(inner)),
                );
            }

            (HirTyKind::Vec(source_inner), HirTyKind::Vec(target_inner)) => {
                return HirTyKind::try_promote_wrapped_type(
                    source_ty,
                    &source_inner,
                    target_inner,
                    is_let_stmt,
                    |inner| HirTyKind::Vec(Box::new(inner)),
                );
            }

            (
                HirTyKind::Result(source_ok, source_err),
                HirTyKind::Result(target_ok, target_err),
            ) => {
                let mut new_ok = (*source_ok).clone();
                let mut new_err = (*source_err).clone();
                let ok_promoted = HirTyKind::try_promote_type(&mut new_ok, target_ok, is_let_stmt);
                let err_promoted =
                    HirTyKind::try_promote_type(&mut new_err, target_err, is_let_stmt);

                if ok_promoted || err_promoted {
                    *source_ty = HirTyKind::Result(Box::new(new_ok), Box::new(new_err));
                    return true;
                }
            }

            (HirTyKind::ResultOk(source_inner), HirTyKind::Result(ok, _)) => {
                return HirTyKind::try_promote_wrapped_type(
                    source_ty,
                    &source_inner,
                    ok,
                    is_let_stmt,
                    |inner| HirTyKind::ResultOk(Box::new(inner)),
                );
            }

            (HirTyKind::ResultErr(source_err), HirTyKind::Result(_, err)) => {
                return HirTyKind::try_promote_wrapped_type(
                    source_ty,
                    &source_err,
                    err,
                    is_let_stmt,
                    |inner| HirTyKind::ResultErr(Box::new(inner)),
                );
            }

            (HirTyKind::Option(source_inner), HirTyKind::Option(target_inner)) => {
                return HirTyKind::try_promote_wrapped_type(
                    source_ty,
                    &source_inner,
                    target_inner,
                    is_let_stmt,
                    |inner| HirTyKind::Option(Box::new(inner)),
                );
            }

            (HirTyKind::Some(source_inner), HirTyKind::Some(target_inner)) => {
                return HirTyKind::try_promote_wrapped_type(
                    source_ty,
                    &source_inner,
                    target_inner,
                    is_let_stmt,
                    |inner| HirTyKind::Some(Box::new(inner)),
                );
            }

            (HirTyKind::ResultOk(source_inner), HirTyKind::ResultOk(target_inner)) => {
                return HirTyKind::try_promote_wrapped_type(
                    source_ty,
                    &source_inner,
                    target_inner,
                    is_let_stmt,
                    |inner| HirTyKind::ResultOk(Box::new(inner)),
                );
            }

            (HirTyKind::ResultErr(source_inner), HirTyKind::ResultErr(target_inner)) => {
                return HirTyKind::try_promote_wrapped_type(
                    source_ty,
                    &source_inner,
                    target_inner,
                    is_let_stmt,
                    |inner| HirTyKind::ResultErr(Box::new(inner)),
                );
            }

            _ => {}
        }

        false
    }

    fn try_promote_wrapped_type<F>(
        source_ty: &mut HirTyKind,
        source_inner: &Box<HirTyKind>,
        target_inner: &Box<HirTyKind>,
        is_let_stmt: bool,
        wrapper_constructor: F,
    ) -> bool
    where
        F: FnOnce(HirTyKind) -> HirTyKind,
    {
        let mut inner_source = (**source_inner).clone();
        if HirTyKind::try_promote_type(&mut inner_source, target_inner, is_let_stmt) {
            *source_ty = wrapper_constructor(inner_source);
            return true;
        }
        false
    }

    pub fn is_void(&self) -> bool {
        match self {
            HirTyKind::Primitive(PrimitiveType::Void) => true,
            HirTyKind::Ref(ty, _)
            | HirTyKind::Ptr(ty, _)
            | HirTyKind::Mut(ty)
            | HirTyKind::Const(ty) => ty.is_void(),
            _ => false,
        }
    }

    pub fn is_void_ptr(&self) -> bool {
        match self {
            HirTyKind::Ptr(ty, _) => ty.is_void(),
            HirTyKind::Ref(ty, _) => ty.is_void(),
            _ => false,
        }
    }
}

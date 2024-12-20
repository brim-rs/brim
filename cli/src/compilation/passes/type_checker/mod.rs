use crate::ast::{statements::TypeAnnotation, types::TypeKind};
use std::fmt::Display;

pub mod pass;

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedType {
    pub kind: TypeKind,
    pub is_nullable: bool,
    pub generics: Vec<ResolvedType>,
    pub can_be_error: bool,
    pub error_type: Option<String>,
}

impl ResolvedType {
    pub fn new(
        kind: TypeKind,
        is_nullable: bool,
        generics: Vec<ResolvedType>,
        can_be_error: bool,
        error_type: Option<String>,
    ) -> Self {
        Self {
            kind,
            is_nullable,
            generics,
            can_be_error,
            error_type,
        }
    }

    pub fn base(kind: TypeKind) -> Self {
        Self {
            kind,
            is_nullable: false,
            generics: vec![],
            can_be_error: false,
            error_type: None,
        }
    }
}

impl ResolvedType {
    pub fn is_number(&self) -> bool {
        matches!(
            self.kind,
            TypeKind::I8
                | TypeKind::I16
                | TypeKind::I32
                | TypeKind::I64
                | TypeKind::I128
                | TypeKind::U8
                | TypeKind::U16
                | TypeKind::U32
                | TypeKind::U64
                | TypeKind::U128
                | TypeKind::F32
                | TypeKind::F64
        )
    }

    pub fn matches(&self, other: &ResolvedType) -> bool {
        self.kind == other.kind
            && self.generics.len() == other.generics.len()
            && self
                .generics
                .iter()
                .zip(other.generics.iter())
                .all(|(a, b)| a.matches(b))
            && self.is_nullable == other.is_nullable
            && self.can_be_error == other.can_be_error
            && self.error_type == other.error_type
    }

    pub fn from_type_annotation(typ: TypeAnnotation) -> ResolvedType {
        let generics = typ
            .generics
            .into_iter()
            .map(ResolvedType::from_type_annotation)
            .collect();

        ResolvedType {
            kind: typ.kind,
            is_nullable: typ.is_nullable,
            generics,
            can_be_error: typ.can_be_error,
            error_type: typ.error_type.map(|t| t.literal()),
        }
    }

    pub fn is_string_like(&self) -> bool {
        matches!(self.kind, TypeKind::String | TypeKind::Char)
    }

    pub fn is_string_or_number(&self) -> bool {
        self.is_string_like() || self.is_number()
    }
    
    pub fn array_type(&self) -> ResolvedType {
        match self.kind {
            TypeKind::Array(ref t) => *t.clone(),
            _ => unreachable!("Expected array type"),
        }
    }
}

impl Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)?;

        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, generic) in self.generics.iter().enumerate() {
                write!(f, "{}", generic)?;
                if i < self.generics.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">")?;
        }

        if self.is_nullable {
            write!(f, "?")?;
        }

        Ok(())
    }
}

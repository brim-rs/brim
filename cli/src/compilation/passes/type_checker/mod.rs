use crate::ast::types::TypeKind;

pub mod pass;

#[derive(Debug, Clone)]
pub struct ResolvedType {
    pub kind: TypeKind,
    pub is_nullable: bool,
    pub generics: Vec<ResolvedType>,
    pub can_be_error: bool,
    pub error_type: Option<String>,
}

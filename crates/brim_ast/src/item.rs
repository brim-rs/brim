use std::fmt::Debug;
use std::path::Path;
use brim_span::span::Span;
use brim_span::symbol::Symbol;
use crate::NodeId;

#[derive(Clone, Debug)]
pub struct Item {
    pub id: NodeId,
    pub span: Span,
    pub vis: Visibility,
    pub ident: Ident,
    pub kind: ItemKind,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.name)
    }
}

#[derive(Clone, Debug)]
pub struct Visibility {
    pub span: Span,
    pub kind: VisibilityKind,
}

#[derive(Clone, Debug)]
pub enum VisibilityKind {
    Public,
    Private,
}

impl VisibilityKind {
    pub fn is_public(&self) -> bool {
        matches!(self, VisibilityKind::Public)
    }
}

#[derive(Clone, Debug)]
pub enum ItemKind {}
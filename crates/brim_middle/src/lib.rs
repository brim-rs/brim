use brim_ast::{
    NodeId,
    item::{FnDecl, Ident},
};
use brim_index::index_type;
use brim_ast::expr::{BinOpKind, UnaryOp};
use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;

pub mod args;
pub mod barrel;
pub mod builtins;
pub mod experimental;
pub mod lints;
pub mod modules;
pub mod temp_diag;
pub mod walker;

index_type! {
    /// A unique identifier for a module in the compiler.
    #[derive(PartialOrd, Ord)]
    pub struct ModuleId {}
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
/// A symbol identifier.
pub struct GlobalSymbolId {
    pub mod_id: ModuleId,
    pub item_id: NodeId,
}

#[derive(Debug, Clone)]
pub struct GlobalSymbol {
    pub id: GlobalSymbolId,
    pub name: Ident,
    pub kind: GlobalSymbolKind,
    pub item_id: NodeId,
}

impl GlobalSymbol {
    pub fn new(name: Ident, kind: GlobalSymbolKind, id: NodeId, gid: GlobalSymbolId) -> Self {
        Self {
            name,
            kind,
            item_id: id,
            id: gid,
        }
    }
}

#[derive(Debug, Clone)]
pub enum GlobalSymbolKind {
    Fn(FnDecl),
}

#[derive(Diagnostic)]
#[error("experimental feature `{feature}` is not enabled on this build")]
pub struct ExperimentalFeatureNotEnabled {
    #[error]
    pub span: (Span, usize),
    pub feature: String,
    #[note]
    pub note: String,
}


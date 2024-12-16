use std::collections::HashMap;
use std::sync::Arc;
use crate::compilation::passes::type_checker::ResolvedType;
use anyhow::Result;
use crate::compilation::imports::UnitLoader;
use crate::compilation::unit::CompilationUnit;
use crate::error::diagnostic::{Diagnostic, Diagnostics};
use crate::error::span::TextSpan;

#[derive(Debug)]
pub struct TypePass<'a> {
    pub scopes: Vec<HashMap<String, ResolvedType>>,
    pub unit: &'a mut CompilationUnit,
    pub loader: &'a mut UnitLoader,
    pub diags: &'a mut Diagnostics,
}

impl<'a> TypePass<'a> {
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare_variable(&mut self, name: String, typ: ResolvedType) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, typ);
        }
    }

    pub fn set_variable(&mut self, name: &str, val: ResolvedType, span: TextSpan) -> Result<()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), val);
                return Ok(());
            }
        }

        self.diags.new_diagnostic(Diagnostic::error(
            format!("Variable '{}' not found in scope", name),
            vec![(span, None)],
            vec![],
        ), Arc::new(self.unit.source.clone()));

        Ok(())
    }

    pub fn find_variable(&self, name: &str) -> Option<&ResolvedType> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }
}
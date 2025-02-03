use crate::HirId;
use brim_ast::{
    NodeId,
    item::Param,
    token::{Lit, LitKind},
};
use brim_diagnostics::{OptionalDiag, box_diag};
use brim_span::span::Span;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct EvalScope {
    variables: HashMap<String, VariableInfo>,
    parent: Option<Box<EvalScope>>,
    file: usize,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub span: Span,
    pub val: Lit,
}

impl<'a> EvalScope {
    pub fn new(file: usize) -> Self {
        Self {
            variables: HashMap::new(),
            parent: None,
            file,
        }
    }

    pub fn new_child(parent: &EvalScope, file: usize) -> Self {
        Self {
            variables: HashMap::new(),
            parent: Some(Box::new(parent.clone())),
            file,
        }
    }

    pub fn declare_variable(&mut self, name: String, info: VariableInfo) -> OptionalDiag<'a> {
        self.variables.insert(name, info);
        None
    }

    pub fn resolve_variable(&self, name: &str) -> Option<(EvalScope, &VariableInfo)> {
        if let Some(var) = self.variables.get(name) {
            return Some((self.clone(), var));
        }

        self.parent
            .as_ref()
            .and_then(|parent| parent.resolve_variable(name))
    }
}

#[derive(Debug)]
pub struct EvalScopeManager {
    scope_stack: Vec<EvalScope>,
}

impl<'a> EvalScopeManager {
    pub fn new(file: usize) -> Self {
        Self {
            scope_stack: vec![EvalScope::new(file)],
        }
    }

    pub fn push_scope(&mut self, file: usize) {
        let current_scope = self.current_scope();
        let new_scope = EvalScope::new_child(current_scope, file);
        self.scope_stack.push(new_scope);
    }

    pub fn pop_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

    pub fn current_scope(&mut self) -> &mut EvalScope {
        self.scope_stack
            .last_mut()
            .expect("Scope stack should never be empty")
    }

    pub fn resolve_variable(&self, name: &str) -> Option<(EvalScope, &VariableInfo)> {
        self.scope_stack
            .last()
            .and_then(|scope| scope.resolve_variable(name))
    }

    pub fn declare_variable(&mut self, name: String, info: VariableInfo) -> OptionalDiag<'a> {
        self.current_scope().declare_variable(name, info)
    }
}

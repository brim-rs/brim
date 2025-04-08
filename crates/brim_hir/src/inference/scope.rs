use crate::ty::HirTyKind;
use brim_span::span::Span;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TypeScope {
    variables: HashMap<String, TypeInfo>,
    parent: Option<Box<TypeScope>>,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub ty: HirTyKind,
    pub span: Span,
}

impl<'a> Default for TypeScope {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> TypeScope {
    pub fn new() -> Self {
        Self { variables: HashMap::new(), parent: None }
    }

    pub fn new_child(parent: &TypeScope) -> Self {
        Self { variables: HashMap::new(), parent: Some(Box::new(parent.clone())) }
    }

    pub fn declare_variable(
        &mut self,
        name: String,
        info: TypeInfo,
        check_duplicates: bool,
    ) -> Option<TypeInfo> {
        if self.variables.contains_key(&name) && check_duplicates {
            let var = self.variables[&name].clone();
            return Some(var);
        }

        self.variables.insert(name, info);
        None
    }

    pub fn resolve_variable(&self, name: &str) -> Option<&TypeInfo> {
        if let Some(var) = self.variables.get(name) {
            return Some(var);
        }

        self.parent.as_ref().and_then(|parent| parent.resolve_variable(name))
    }
}

#[derive(Debug, Clone)]
pub struct TypeScopeManager {
    scope_stack: Vec<TypeScope>,
}

impl<'a> Default for TypeScopeManager {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> TypeScopeManager {
    pub fn new() -> Self {
        Self { scope_stack: vec![TypeScope::new()] }
    }

    pub fn push_scope(&mut self) {
        let current_scope = self.current_scope();
        let new_scope = TypeScope::new_child(current_scope);
        self.scope_stack.push(new_scope);
    }

    pub fn pop_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

    pub fn current_scope(&mut self) -> &mut TypeScope {
        self.scope_stack.last_mut().expect("TypeScope stack should never be empty")
    }

    pub fn resolve_variable(&self, name: &str) -> Option<&TypeInfo> {
        self.scope_stack.last().and_then(|scope| scope.resolve_variable(name))
    }

    pub fn declare_variable(
        &mut self,
        name: String,
        info: TypeInfo,
        check_duplicates: bool,
    ) -> Option<TypeInfo> {
        self.current_scope().declare_variable(name, info, check_duplicates)
    }
}

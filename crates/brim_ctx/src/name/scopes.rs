use brim_ast::{ItemId, item::Param};
use brim_diag_macro::Diagnostic;
use brim_diagnostics::{
    OptionalDiag, box_diag,
    diagnostic::{Label, LabelStyle, Severity, ToDiagnostic},
};
use brim_span::span::Span;
use std::collections::HashMap;

#[derive(Diagnostic)]
#[error("found redeclaration of variable `{name}`")]
pub struct RedeclaredVariable {
    #[error("first defined here")]
    pub span: (Span, usize),
    #[error("redeclared here")]
    pub redecl: (Span, usize),
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Scope {
    variables: HashMap<String, VariableInfo>,
    parent: Option<Box<Scope>>,
    file: usize,
    pub inside_comptime: bool,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub id: ItemId,
    pub is_const: bool,
    pub span: Span,
}

impl From<Param> for VariableInfo {
    fn from(param: Param) -> Self {
        Self {
            id: param.id,
            is_const: param.ty.is_const(),
            span: param.span,
        }
    }
}

impl<'a> Scope {
    pub fn new(file: usize) -> Self {
        Self {
            variables: HashMap::new(),
            parent: None,
            file,
            inside_comptime: false,
        }
    }

    pub fn new_child(parent: &Scope, file: usize, inside_comptime: bool) -> Self {
        Self {
            variables: HashMap::new(),
            parent: Some(Box::new(parent.clone())),
            file,
            inside_comptime,
        }
    }

    pub fn declare_variable(
        &mut self,
        name: String,
        info: VariableInfo,
        check_duplicates: bool,
    ) -> OptionalDiag<'a> {
        if self.variables.contains_key(&name) && check_duplicates {
            let var = self.variables.get(&name).unwrap();

            box_diag!(@opt RedeclaredVariable {
                span: (var.span, self.file),
                redecl: (info.span, self.file),
                name
            });
        }

        self.variables.insert(name, info);
        None
    }

    // Resolve a variable, checking current and parent scopes
    pub fn resolve_variable(&self, name: &str) -> Option<(Scope, &VariableInfo)> {
        // First check in current scope
        if let Some(var) = self.variables.get(name) {
            return Some((self.clone(), var));
        }

        // If not found, check parent scopes
        self.parent
            .as_ref()
            .and_then(|parent| parent.resolve_variable(name))
    }
}

#[derive(Debug)]
pub struct ScopeManager {
    scope_stack: Vec<Scope>,
}

impl<'a> ScopeManager {
    pub fn new(file: usize) -> Self {
        Self {
            scope_stack: vec![Scope::new(file)],
        }
    }

    pub fn push_scope(&mut self, file: usize, inside: bool) {
        let current_scope = self.current_scope();
        let new_scope = Scope::new_child(current_scope, file, inside);
        self.scope_stack.push(new_scope);
    }

    pub fn pop_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

    pub fn current_scope(&mut self) -> &mut Scope {
        self.scope_stack
            .last_mut()
            .expect("Scope stack should never be empty")
    }

    pub fn resolve_variable(&self, name: &str) -> Option<(Scope, &VariableInfo)> {
        self.scope_stack
            .last()
            .and_then(|scope| scope.resolve_variable(name))
    }

    pub fn declare_variable(
        &mut self,
        name: String,
        info: VariableInfo,
        check_duplicates: bool,
    ) -> OptionalDiag<'a> {
        self.current_scope()
            .declare_variable(name, info, check_duplicates)
    }
}

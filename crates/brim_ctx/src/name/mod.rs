mod errors;
pub mod scopes;

use crate::name::{
    errors::{
        AccessOutsideComptime, InvalidPathAccess, NamespaceMissingSymbol, UndeclaredFunction,
        UndeclaredStruct, UndeclaredVariable,
    },
    scopes::Scope,
};
use brim_ast::{
    ItemId,
    expr::{Expr, ExprKind, MatchArm},
    item::{Block, FnDecl, Ident, Item, ItemKind, Struct, TypeAlias, TypeAliasValue},
    stmts::Let,
};
use brim_diagnostics::diag_opt;
use brim_hir::CompiledModules;
use brim_middle::{
    SimpleModules, builtins::BUILTIN_FUNCTIONS, lints::Lints, modules::ModuleMap,
    temp_diag::TemporaryDiagnosticContext, walker::AstWalker,
};
use brim_span::span::Span;
use convert_case::{Case, Casing};
use errors::{
    ItemNotAMethodInStruct, NoItemInStruct, StaticCallToMethodInStruct, UndeclaredStructStatic,
};
use scopes::{ScopeManager, VariableInfo};
use tracing::debug;

#[derive(Debug)]
pub struct NameResolver<'a> {
    pub ctx: TemporaryDiagnosticContext,
    pub map: ModuleMap,
    pub scopes: ScopeManager,
    pub file: usize,
    pub lints: &'static Lints,
    pub inside_comptime: bool,
    pub compiled: &'a mut CompiledModules,
    pub external: bool,
    pub simple: &'a mut SimpleModules,
}

impl<'a> NameResolver<'a> {
    pub fn new(
        map: ModuleMap,
        lints: &'static Lints,
        compiled: &'a mut CompiledModules,
        simple: &'a mut SimpleModules,
    ) -> Self {
        Self {
            ctx: TemporaryDiagnosticContext::new(),
            map,
            scopes: ScopeManager::new(0),
            file: 0,
            lints,
            inside_comptime: false,
            compiled,
            external: false,
            simple,
        }
    }

    pub fn resolve_names(&mut self) {
        for module in self.map.modules.clone() {
            debug!("Resolving names for module: {:?}", module.barrel.file_id);

            self.file = module.barrel.file_id;
            for mut item in module.barrel.items {
                self.walk_item(&mut item);
            }
        }
    }

    /// declare_param doesn't check for duplicates, because that is already handled by the
    /// [`AstValidator`](crate::validator::AstValidator)
    fn declare_param(&mut self, name: &str, info: VariableInfo) {
        diag_opt!(
            self.ctx,
            self.scopes.declare_variable(name.to_string(), info, false)
        )
    }

    fn declare_variable(&mut self, name: &str, info: VariableInfo) {
        diag_opt!(
            self.ctx,
            self.scopes.declare_variable(name.to_string(), info, true)
        )
    }

    // Check if a variable is declared in any accessible scope
    pub fn is_variable_declared(&self, name: &str) -> Option<(Scope, &VariableInfo)> {
        self.scopes.resolve_variable(name)
    }

    pub fn validate_var_name(&mut self, name: &str, span: Span) {
        let snake = name.to_case(Case::Snake);
        if name != snake && !self.external {
            self.ctx.emit_lint(self.lints.variable_not_snake_case(
                name.to_string(),
                snake,
                (span, self.file),
            ));
        }
    }

    pub fn resolve_path(&mut self, idents: Vec<Ident>, expr_id: ItemId, from_call: bool) {
        if idents.is_empty() {
            return;
        }

        let name = idents[0].to_string();
        let span = (idents[0].span, self.file);

        match self.compiled.symbols.resolve(&name, self.file) {
            Some(item) => {
                let item = self.simple.get_item(item.id.item_id);

                if from_call {
                    match &item.kind {
                        ItemKind::Struct(_) => self.compiled.assign_path(expr_id, item.id),
                        _ => {
                            self.ctx.emit_impl(UndeclaredStructStatic { span, name });
                        }
                    }
                } else if idents.len() > 1 {
                    let second_ident = &idents[1];
                    let second_name = second_ident.to_string();

                    match &item.kind {
                        ItemKind::Namespace(symbols) => {
                            if let Some(sym) = symbols.get(&second_name) {
                                self.compiled.assign_path(expr_id, sym.1.0);
                            } else {
                                self.ctx.emit_impl(NamespaceMissingSymbol {
                                    span: (second_ident.span, self.file),
                                    name,
                                    symbol: second_name,
                                });
                            }
                        }
                        _ => {
                            self.ctx.emit_impl(InvalidPathAccess { span, name });
                        }
                    }
                } else {
                    self.compiled.assign_path(expr_id, item.id);
                }
            }
            None => {
                if from_call {
                    self.ctx.emit_impl(UndeclaredStructStatic { span, name });
                } else {
                    self.ctx.emit_impl(InvalidPathAccess { span, name });
                }
            }
        }
    }
}

impl<'a> AstWalker for NameResolver<'a> {
    fn walk_item(&mut self, item: &mut Item) {
        match &mut item.kind {
            ItemKind::Fn(func) => self.visit_fn(func),
            ItemKind::Use(use_stmt) => self.visit_use(use_stmt),
            ItemKind::Struct(str) => self.visit_struct(str),
            ItemKind::TypeAlias(type_alias) => self.visit_type_alias(type_alias),
            ItemKind::Module(_) => {}
            ItemKind::External(external) => {
                self.external = true;
                for item in &mut external.items {
                    match &mut item.kind {
                        ItemKind::Fn(func) => self.visit_fn(func),
                        ItemKind::TypeAlias(ty) => self.visit_type_alias(ty),
                        _ => unreachable!("not allowed"),
                    }
                }
                self.external = false;
            }
            ItemKind::Namespace(_) => unreachable!(),
        }
    }

    fn visit_struct(&mut self, str: &mut Struct) {
        for item in &mut str.items {
            self.visit_item(item);
        }
    }

    fn visit_let(&mut self, let_stmt: &mut Let) {
        let name = let_stmt.ident.to_string();

        self.declare_variable(&name, VariableInfo {
            id: let_stmt.id,
            is_const: if let Some(ty) = &let_stmt.ty {
                ty.is_const()
            } else {
                false
            },
            span: let_stmt.span,
        });

        self.validate_var_name(&name, let_stmt.ident.span);

        if let Some(expr) = &mut let_stmt.value {
            self.walk_expr(expr);
        }
    }

    fn visit_type_alias(&mut self, ta: &mut TypeAlias) {
        match &mut ta.ty {
            TypeAliasValue::Const(expr) => {
                self.walk_expr(expr);
            }
            _ => {}
        }
    }

    fn visit_block(&mut self, block: &mut Block) {
        self.scopes.push_scope(self.file, self.inside_comptime);

        for stmt in block.stmts.iter_mut() {
            self.visit_stmt(stmt);
        }

        self.scopes.pop_scope();
    }

    fn visit_fn(&mut self, func: &mut FnDecl) {
        self.scopes = ScopeManager::new(self.file);

        let name = func.sig.name.to_string();
        let camel = name.to_case(Case::Camel);

        if name != camel && !self.external {
            self.ctx.emit_lint(self.lints.function_not_camel_case(
                name.to_string(),
                camel,
                (func.sig.name.span, self.file),
            ));
        }

        for param in &func.sig.params {
            self.declare_param(&param.name.to_string(), VariableInfo {
                id: param.id,
                is_const: false,
                span: param.span,
            });

            self.validate_var_name(&param.name.to_string(), param.name.span);
        }

        if let Some(body) = &mut func.body {
            self.visit_block(body);
        }
    }

    fn walk_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::Binary(lhs, _, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::Unary(_, operand) => self.visit_expr(operand),
            ExprKind::Field(base, _) => self.visit_expr(base),
            ExprKind::Index(base, index) => {
                self.visit_expr(base);
                self.visit_expr(index);
            }
            ExprKind::Literal(_) => {}
            ExprKind::Paren(inner) => self.visit_expr(inner),
            ExprKind::Return(inner) => self.visit_expr(inner),
            ExprKind::Var(ident) => {
                let var_name = ident.name.to_string();
                let scope = self.is_variable_declared(&var_name);

                if let Some((scope, var)) = scope {
                    if self.inside_comptime && !scope.inside_comptime {
                        self.ctx.emit_impl(AccessOutsideComptime {
                            span: (ident.span, self.file),
                            name: var_name,
                            decl: (var.span, self.file),
                        });
                    }
                } else {
                    self.ctx.emit_impl(UndeclaredVariable {
                        span: (ident.span, self.file),
                        name: var_name.clone(),
                    });
                }
            }
            ExprKind::AssignOp(lhs, _, rhs) | ExprKind::Assign(lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::If(if_expr) => {
                self.visit_expr(&mut if_expr.condition);
                self.visit_expr(&mut if_expr.then_block);

                for else_if in &mut if_expr.else_ifs {
                    self.visit_expr(&mut else_if.condition);
                    self.visit_expr(&mut else_if.block);
                }

                if let Some(else_branch) = &mut if_expr.else_block {
                    self.visit_expr(else_branch);
                }
            }
            ExprKind::Block(block) => self.visit_block(block),
            ExprKind::Call(func, args) => {
                let name = func.as_ident().unwrap().to_string();

                let func_sym = self.compiled.symbols.resolve(&name, self.file);

                if let None = func_sym {
                    self.ctx.emit(Box::new(UndeclaredFunction {
                        span: (func.span, self.file),
                        name,
                    }));
                }

                for arg in args {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Comptime(expr) => {
                self.inside_comptime = true;
                self.visit_expr(expr);
                self.inside_comptime = false;
            }
            ExprKind::Array(items) => {
                for item in items {
                    self.visit_expr(item);
                }
            }
            ExprKind::Builtin(ident, args) => {
                let name = ident.to_string();
                let func = BUILTIN_FUNCTIONS.get(&name);

                if let None = func {
                    self.ctx.emit(Box::new(UndeclaredFunction {
                        span: (ident.span, self.file),
                        name,
                    }));
                }

                for arg in args {
                    self.visit_expr(arg);
                }
            }
            ExprKind::StructConstructor(ident, _, fields) => {
                let name = ident.to_string();
                let func_sym = self.compiled.symbols.resolve(&name, self.file);

                if let None = func_sym {
                    self.ctx.emit(Box::new(UndeclaredStruct {
                        span: (ident.span, self.file),
                        name,
                    }));
                }

                for (_, field) in fields {
                    self.visit_expr(field);
                }
            }
            ExprKind::StaticAccess(ident, expr) => {
                self.resolve_path(ident.clone(), expr.id, true);

                let assigned = self.compiled.get_assigned_path(expr.id);
                let str = self.simple.get_item(assigned);

                if let ExprKind::Call(ident, _) = &expr.kind {
                    let ident = ident.as_ident().unwrap().clone();
                    let item = str.kind.as_struct().unwrap().find_item(ident);

                    if let Some(item) = item {
                        if let ItemKind::Fn(func) = &item.kind {
                            if !func.is_static() {
                                self.ctx.emit_impl(StaticCallToMethodInStruct {
                                    span: (ident.span, self.file),
                                    struct_name: str.ident.to_string(),
                                });
                            }
                        } else {
                            self.ctx.emit_impl(ItemNotAMethodInStruct {
                                span: (ident.span, self.file),
                                name: ident.name.to_string(),
                                struct_name: str.ident.to_string(),
                            });
                        }
                    } else {
                        self.ctx.emit_impl(NoItemInStruct {
                            span: (ident.span, self.file),
                            name: ident.name.to_string(),
                            struct_name: str.ident.to_string(),
                        });
                    }
                } else {
                    todo!("error message")
                }
            }
            ExprKind::Match(expr, arms) => {
                self.visit_expr(expr);
                for arm in arms {
                    match arm {
                        MatchArm::Case(pat, block) => {
                            self.visit_expr(pat);
                            self.visit_expr(block);
                        }
                        MatchArm::Else(block) => {
                            self.visit_expr(block);
                        }
                    }
                }
            }
            ExprKind::Path(idents) => {
                self.resolve_path(idents.clone(), expr.id, false);
            }
            ExprKind::Type(ty) => {
                self.visit_ty(ty);
            }
        }
    }
}

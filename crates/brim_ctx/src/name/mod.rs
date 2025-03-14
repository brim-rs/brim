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
    expr::{Expr, ExprKind, MatchArm},
    item::{Block, FnDecl, TypeAlias, TypeAliasValue},
    stmts::Let,
};
use brim_diagnostics::diag_opt;
use brim_hir::{CompiledModules, items::HirItemKind};
use brim_middle::{
    builtins::BUILTIN_FUNCTIONS, lints::Lints, modules::ModuleMap,
    temp_diag::TemporaryDiagnosticContext, walker::AstWalker,
};
use brim_span::span::Span;
use convert_case::{Case, Casing};
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
}

impl<'a> NameResolver<'a> {
    pub fn new(map: ModuleMap, lints: &'static Lints, compiled: &'a mut CompiledModules) -> Self {
        Self {
            ctx: TemporaryDiagnosticContext::new(),
            map,
            scopes: ScopeManager::new(0),
            file: 0,
            lints,
            inside_comptime: false,
            compiled,
        }
    }

    pub fn resolve_names(&mut self) {
        for module in self.map.modules.clone() {
            debug!("Resolving names for module: {:?}", module.barrel.file_id);

            self.file = module.barrel.file_id;
            for mut item in module.barrel.items {
                self.visit_item(&mut item);
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
        if name != snake {
            self.ctx.emit_lint(self.lints.variable_not_snake_case(
                name.to_string(),
                snake,
                (span, self.file),
            ));
        }
    }
}

impl<'a> AstWalker for NameResolver<'a> {
    fn visit_let(&mut self, let_stmt: &mut Let) {
        let name = let_stmt.ident.to_string();

        self.declare_variable(
            &name,
            VariableInfo {
                id: let_stmt.id,
                is_const: if let Some(ty) = &let_stmt.ty {
                    ty.is_const()
                } else {
                    false
                },
                span: let_stmt.span,
            },
        );

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

        if name != camel {
            self.ctx.emit_lint(self.lints.function_not_camel_case(
                name.to_string(),
                camel,
                (func.sig.name.span, self.file),
            ));
        }

        for param in &func.sig.params {
            self.declare_param(
                &param.name.to_string(),
                VariableInfo {
                    id: param.id,
                    is_const: false,
                    span: param.span,
                },
            );

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
                let item = self
                    .compiled
                    .symbols
                    .resolve(&idents[0].to_string(), self.file);

                if let Some(item) = item {
                    let item = self.compiled.get_item(item.id.item_id);

                    match &item.kind {
                        HirItemKind::Namespace(symbols) => {
                            if let Some(sym) = symbols.get(&idents[1].to_string()) {
                                self.compiled.assign_path(expr.id, sym.id.item_id);
                            } else {
                                self.ctx.emit_impl(NamespaceMissingSymbol {
                                    span: (idents[1].span, self.file),
                                    name: idents[0].to_string(),
                                    symbol: idents[1].to_string(),
                                });
                            }
                        }
                        _ => {
                            self.ctx.emit_impl(InvalidPathAccess {
                                span: (idents[0].span, self.file),
                                name: idents[0].to_string(),
                            });
                        }
                    }
                } else {
                    self.ctx.emit_impl(InvalidPathAccess {
                        span: (idents[0].span, self.file),
                        name: idents[0].to_string(),
                    });
                }
            }
            ExprKind::Type(ty) => {
                println!("{:#?}", ty.kind);
                self.visit_ty(ty);
            }
        }
    }
}

mod errors;
mod scopes;

use crate::name::{
    errors::{UndeclaredFunction, UndeclaredVariable},
    scopes::{ScopeManager, VariableInfo},
};
use brim_ast::{
    expr::{Expr, ExprKind},
    item::{Block, FnDecl},
};
use brim_ctx::{ModuleId, compiler::CompilerContext, modules::ModuleMap, walker::AstWalker};
use brim_diagnostics::diag_opt;
use tracing::debug;

#[derive(Debug)]
pub struct NameResolver<'a> {
    pub ctx: &'a mut CompilerContext<'a>,
    pub map: ModuleMap,
    pub scopes: ScopeManager,
    pub file: usize,
}

impl<'a> NameResolver<'a> {
    pub fn new(ctx: &'a mut CompilerContext<'a>, map: ModuleMap) -> Self {
        Self {
            ctx,
            map,
            scopes: ScopeManager::new(0),
            file: 0,
        }
    }

    pub fn resolve_names(&mut self) {
        for module in self.map.modules.clone() {
            debug!("AST validating module: {:?}", module.barrel.file_id);

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
    pub fn is_variable_declared(&self, name: &str) -> bool {
        self.scopes.resolve_variable(name).is_some()
    }
}

impl<'a> AstWalker for NameResolver<'a> {
    fn visit_block(&mut self, block: &mut Block) {
        self.scopes.push_scope(self.file);

        for stmt in block.stmts.iter_mut() {
            self.visit_stmt(stmt);
        }

        self.scopes.pop_scope();
    }

    fn visit_fn(&mut self, func: &mut FnDecl) {
        self.scopes = ScopeManager::new(self.file);

        for param in &func.sig.params {
            self.declare_param(&param.name.to_string(), VariableInfo {
                id: param.id,
                is_const: false,
                span: param.span,
            });
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
            ExprKind::Var(var) => {
                if !self.is_variable_declared(&var.name.to_string()) {
                    self.ctx.emit(UndeclaredVariable {
                        span: (var.span, self.file),
                        name: var.name.to_string(),
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
                let mod_id = ModuleId::from_usize(self.file);

                let func_sym = self.map.resolve_symbol(&name, mod_id);

                if let None = func_sym {
                    self.ctx.emit(UndeclaredFunction {
                        span: (func.span, self.file),
                        name,
                    });
                }

                for arg in args {
                    self.visit_expr(arg);
                }
            }
        }
    }
}

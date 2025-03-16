pub mod errors;
mod eval_scopes;

use crate::{
    CompiledModules,
    comptime::{
        errors::NoValueReturned,
        eval_scopes::{EvalScopeManager, VariableInfo},
    },
    errors::ComptimeExpectedType,
    expr::{ComptimeValue, HirBlock, HirExpr, HirExprKind, HirMatchArm},
    items::{HirFn, HirItemKind, HirTypeAlias},
    stmts::{HirStmt, HirStmtKind},
    transformer::{HirModule, HirModuleMap, StoredHirItem, Transformer},
    ty::HirTyKind,
};
use brim_ast::{
    Empty, ItemId,
    expr::Expr,
    item::Ident,
    token::{Lit, LitKind},
};
use brim_middle::{ModuleId, temp_diag::TemporaryDiagnosticContext};

#[derive(Debug, Clone)]
pub enum ComptimeReturnValue {
    Lit(Lit),
    Ty(HirTyKind),
}

impl ComptimeReturnValue {
    pub fn as_lit(&self) -> &Lit {
        match self {
            ComptimeReturnValue::Lit(lit) => lit,
            _ => unreachable!(),
        }
    }

    pub fn as_ty(&self) -> &HirTyKind {
        match self {
            ComptimeReturnValue::Ty(ty) => ty,
            _ => unreachable!(),
        }
    }
}

impl<'a> Transformer<'a> {
    pub fn transform_comptime_expr(&mut self, expr: Expr) -> ComptimeReturnValue {
        let (expr, _) = self.transform_expr(expr);

        Evaluator::new(self.current_mod_id, self.compiled).eval_expr(expr)
    }
}

pub fn transform_comptime<'a>(
    hir: &'a mut HirModuleMap,
    compiled: &'a mut CompiledModules,
) -> ComptimeTransformer<'a> {
    let mut transformer = ComptimeTransformer {
        hir,
        current_mod: ModuleId::from_usize(0),
        temp: TemporaryDiagnosticContext::new(),
        compiled,
    };

    transformer.eval();

    transformer
}

#[derive(Debug)]
/// Transforms comptime expressions that couldn't be evaulated in the hir transformer.
pub struct ComptimeTransformer<'a> {
    pub hir: &'a mut HirModuleMap,
    pub current_mod: ModuleId,
    pub temp: TemporaryDiagnosticContext,
    pub compiled: &'a mut CompiledModules,
}

impl<'a> ComptimeTransformer<'a> {
    pub fn eval(&mut self) {
        let inferred_modules: Vec<_> = self
            .hir
            .modules
            .clone()
            .into_iter()
            .map(|mut module| {
                self.current_mod = module.mod_id;
                self.eval_module(&mut module);
                module
            })
            .collect();

        self.hir.modules = inferred_modules;
    }

    pub fn eval_module(&mut self, module: &mut HirModule) {
        for item in &mut module.items {
            self.visit_item(item);
        }
    }

    fn visit_item(&mut self, item: &mut ItemId) {
        let mut item = self.compiled.items.get(&item).unwrap().clone();

        match &mut item.kind {
            HirItemKind::Fn(func) => self.visit_fn(func),
            HirItemKind::TypeAlias(type_alias) => self.visit_type_alias(type_alias),
            HirItemKind::Namespace(_) | HirItemKind::Use(_) | HirItemKind::Struct(_) => {}
            HirItemKind::External(_) => {}
        }

        self.hir
            .hir_items
            .insert(item.id, StoredHirItem::Item(item.clone()));
        self.compiled.items.insert(item.id, item);
    }

    fn visit_type_alias(&mut self, type_alias: &mut HirTypeAlias) {
        if let ComptimeValue::Expr(expr) = &mut type_alias.ty {
            type_alias.ty = match self.transform_comptime_expr(*expr.clone()) {
                ComptimeReturnValue::Ty(ty) => ComptimeValue::Resolved(ComptimeReturnValue::Ty(ty)),
                _ => {
                    self.temp.emit_impl(ComptimeExpectedType {
                        span: (expr.span, self.current_mod.as_usize()),
                    });

                    ComptimeValue::Resolved(ComptimeReturnValue::Ty(HirTyKind::void()))
                }
            }
        }
    }

    fn transform_comptime_expr(&mut self, expr: HirExpr) -> ComptimeReturnValue {
        Evaluator::new(self.current_mod, self.compiled).eval_expr(expr)
    }

    fn visit_fn(&mut self, func: &mut HirFn) {}

    fn visit_let(
        &mut self,
        ident: &mut Ident,
        ty: &mut Option<HirTyKind>,
        value: &mut Option<HirExpr>,
    ) {
        if let Some(value) = value {
            self.visit_expr(value);
        }
    }

    fn visit_stmt(&mut self, stmt: &mut HirStmt) {
        match &mut stmt.kind {
            HirStmtKind::Let { ident, ty, value } => self.visit_let(ident, ty, value),
            HirStmtKind::Expr(expr) => self.visit_expr(expr),
        }
    }

    fn visit_expr(&mut self, expr: &mut HirExpr) {
        match &mut expr.kind {
            HirExprKind::Binary(lhs, _, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            HirExprKind::Unary(_, operand) => self.visit_expr(operand),
            HirExprKind::Field(base, _) => self.visit_expr(base),
            HirExprKind::Index(base, index) => {
                self.visit_expr(base);
                self.visit_expr(index);
            }
            HirExprKind::Literal(_) => {}
            HirExprKind::Return(inner) => self.visit_expr(inner),
            HirExprKind::Var(_) => {}
            HirExprKind::Assign(lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            HirExprKind::If(if_expr) => {
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
            HirExprKind::Block(block) => self.visit_block(block),
            HirExprKind::Call(func, args, _) => {
                self.visit_expr(func);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            HirExprKind::Comptime(inner) => {
                if let ComptimeValue::Expr(expr) = inner {
                    *inner = ComptimeValue::Resolved(self.transform_comptime_expr(*expr.clone()));
                }
            }
            HirExprKind::Array(items) => {
                for item in items {
                    self.visit_expr(item);
                }
            }

            HirExprKind::StructConstructor(constructor) => {
                for (_, field) in &mut constructor.fields {
                    self.visit_expr(field);
                }
            }
            HirExprKind::Match(expr, arms) => {
                self.visit_expr(expr);
                for arm in arms {
                    match arm {
                        HirMatchArm::Case(pat, block) => {
                            self.visit_expr(pat);
                            self.visit_expr(block);
                        }
                        HirMatchArm::Else(block) => {
                            self.visit_expr(block);
                        }
                    }
                }
            }
            HirExprKind::Path(_) | HirExprKind::Builtin(_, _) | HirExprKind::Type(_) => {}
        }
    }

    fn visit_block(&mut self, block: &mut HirBlock) {
        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
    }
}

/// For now only literals will be able to return from comptime expressions.
#[derive(Debug)]
pub struct Evaluator<'a> {
    pub scopes: EvalScopeManager,
    pub last_val: Option<ComptimeReturnValue>,
    pub mod_id: usize,
    pub temp: TemporaryDiagnosticContext,
    pub compiled: &'a mut CompiledModules,
}

impl<'a> Evaluator<'a> {
    pub fn new(mod_id: ModuleId, compiled: &'a mut CompiledModules) -> Self {
        Self {
            scopes: EvalScopeManager::new(0),
            last_val: None,
            mod_id: mod_id.as_usize(),
            temp: TemporaryDiagnosticContext::new(),
            compiled,
        }
    }
}

impl<'a> Evaluator<'a> {
    pub fn eval_block(&mut self, block: &HirBlock) -> ComptimeReturnValue {
        self.scopes.push_scope(self.mod_id);
        for stmt in block.stmts.clone() {
            match stmt.kind {
                HirStmtKind::Expr(expr) => {
                    self.eval_expr(expr);
                }
                HirStmtKind::Let { value, ident, .. } => {
                    let val = if let Some(val) = value {
                        self.eval_expr(val)
                    } else {
                        ComptimeReturnValue::Lit(Lit::new(LitKind::None, Empty, None))
                    };
                    self.scopes
                        .declare_variable(ident.name.to_string(), VariableInfo {
                            span: ident.span,
                            val,
                        });
                }
            };
        }
        self.scopes.pop_scope();

        if let Some(val) = self.last_val.clone() {
            val
        } else {
            let err = self.temp.emit_impl(NoValueReturned {
                span: (block.span, self.mod_id),
            });

            ComptimeReturnValue::Lit(Lit::new(LitKind::Err(err), Empty, None))
        }
    }

    pub fn eval_expr(&mut self, expr: HirExpr) -> ComptimeReturnValue {
        let lit = match &expr.kind {
            HirExprKind::Literal(lit) => ComptimeReturnValue::Lit(lit.clone()),
            HirExprKind::Block(block) => self.eval_block(block),
            HirExprKind::Match(expr, arms) => {
                let ret = self.eval_expr(*expr.clone());

                let mut found_match = false;
                let mut lit: Option<ComptimeReturnValue> = None;
                for arm in arms {
                    match arm {
                        HirMatchArm::Case(pat, expr) => {
                            let pat = self.eval_expr(pat.clone());

                            if pat.as_lit() == ret.as_lit() {
                                found_match = true;
                                lit = Some(self.eval_expr(expr.clone()));
                            }
                        }
                        HirMatchArm::Else(expr) => {
                            if !found_match {
                                lit = Some(self.eval_expr(expr.clone()));
                            }
                        }
                    }
                }

                lit.expect("No match arm found")
            }
            HirExprKind::Type(ty) => ComptimeReturnValue::Ty(ty.clone()),
            HirExprKind::Path(id) => {
                let item = self.compiled.get_item(id.clone());

                if let HirItemKind::TypeAlias(alias) = &item.kind {
                    if let ComptimeValue::Resolved(val) = &alias.ty {
                        val.clone()
                    } else {
                        unreachable!()
                    }
                } else {
                    todo!("Resolve path to item")
                }
            }
            _ => unimplemented!("Comptime evaluation for {:?}", expr),
        };

        self.last_val = Some(lit.clone());
        lit
    }
}

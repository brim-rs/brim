use crate::{
    Codegen, HirId,
    expr::{HirExpr, HirExprKind},
    items::{HirItem, HirItemKind},
    stmts::{HirStmt, HirStmtKind},
    transformer::{HirModule, HirModuleMap, StoredHirItem},
    ty::HirTyKind,
};

#[derive(Debug, Clone)]
pub struct BuiltInFunction {
    pub func: fn(&mut Vec<HirExpr>) -> HirExpr,
    pub codegen: fn(&mut dyn Codegen, &mut Vec<HirExpr>) -> String,
}

#[macro_export]
macro_rules! builtin_function {
    (fn $name:ident($($arg:ident),* $(, ...$rest:ident)?) {$($body:tt)*}
     codegen($cg_ctx:ident) {$($cg_body:tt)*}) => {
        #[allow(unused_mut, unused_variables)]
        pub fn $name() -> BuiltInFunction {
            fn inner(args: &mut Vec<HirExpr>) -> HirExpr {
                let mut iter = args.iter_mut();
                $(let mut $arg = iter.next().unwrap();)*
                $(let $rest = iter.collect::<Vec<_>>();)?

                $($body)*
            }

            fn codegen_inner($cg_ctx: &mut dyn Codegen, args: &mut Vec<HirExpr>) -> String {
                let mut iter = args.iter_mut();
                $(let mut $arg = iter.next().unwrap();)*
                $(let $rest = iter.collect::<Vec<_>>();)?

                $($cg_body)*
            }

            BuiltInFunction {
                func: inner,
                codegen: codegen_inner,
            }
        }
    };
}

builtin_function! {
    fn ok(value) {
        value.ty = HirTyKind::ResOk(Box::new(value.ty.clone()));

        value.clone()
    }
    codegen(ctx) {
        format!("{}", ctx.generate_expr(value.clone()))
    }
}

builtin_function! {
    fn err(value) {
        value.ty = HirTyKind::ResErr(Box::new(value.ty.clone()));

        value.clone()
    }
    codegen(ctx) {
        format!("std::unexpected({})", ctx.generate_expr(value.clone()))
    }
}

pub fn get_builtin_function(name: &str) -> Option<BuiltInFunction> {
    match name {
        "ok" => Some(ok()),
        "err" => Some(err()),
        _ => None,
    }
}

pub fn expand_builtins(hir: &mut HirModuleMap) {
    let mut expander = BuiltInExpander { hir };

    expander.expand();
}

#[derive(Debug)]
pub struct BuiltInExpander<'a> {
    pub hir: &'a mut HirModuleMap,
}

impl<'a> BuiltInExpander<'a> {
    pub fn expand(&mut self) {
        let expanded_module: Vec<_> = self
            .hir
            .modules
            .clone()
            .into_iter()
            .map(|mut module| {
                self.expand_module(&mut module);
                module
            })
            .collect();

        self.hir.modules = expanded_module;
    }

    pub fn expand_module(&mut self, module: &mut HirModule) {
        for item in &mut module.items {
            self.expand_item(item);
        }
    }

    fn expand_item(&mut self, item: &mut HirItem) {
        match &mut item.kind {
            HirItemKind::Fn(f) => {
                if let Some(body_id) = f.body {
                    self.expand_body(body_id);
                }
            }
            _ => {}
        }

        self.hir
            .hir_items
            .insert(item.id, StoredHirItem::Item(item.clone()));
    }

    fn expand_body(&mut self, body_id: HirId) {
        let mut expr = self.hir.get_expr_mut(body_id).clone();

        self.expand_expr(&mut expr);
        *self.hir.get_expr_mut(body_id) = expr;
    }

    fn expand_stmt(&mut self, stmt: &mut HirStmt) {
        match &mut stmt.kind {
            HirStmtKind::Expr(expr) => self.expand_expr(expr),
            HirStmtKind::Let { value, .. } => {
                if let Some(value) = value {
                    self.expand_expr(value);
                }
            }
        }

        self.hir
            .hir_items
            .insert(stmt.id, StoredHirItem::Stmt(stmt.clone()));
    }

    fn expand_expr(&mut self, expr: &mut HirExpr) {
        let mut fn_name: Option<String> = None;
        match &mut expr.kind {
            HirExprKind::Unary(_, operand) => {
                self.expand_expr(operand);
            }
            HirExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.expand_stmt(stmt);
                }
            }
            HirExprKind::Return(expr) => {
                self.expand_expr(expr);
            }
            HirExprKind::Binary(lhs, _, rhs) => {
                self.expand_expr(lhs);
                self.expand_expr(rhs);
            }
            HirExprKind::Call(_, args, _) => {
                for arg in args {
                    self.expand_expr(arg);
                }
            }
            HirExprKind::If(if_expr) => {
                self.expand_expr(&mut if_expr.condition);
                self.expand_expr(&mut if_expr.then_block);

                if let Some(else_block) = &mut if_expr.else_block {
                    self.expand_expr(else_block);
                }

                for branch in &mut if_expr.else_ifs {
                    self.expand_expr(&mut branch.condition);
                    self.expand_expr(&mut branch.block);
                }
            }
            HirExprKind::Builtin(name, args) => {
                let func = get_builtin_function(&name.to_string());

                if let Some(func) = func {
                    let x = (func.func)(args);

                    fn_name = Some(name.to_string());
                    *expr = x.clone();
                }
            }
            _ => {}
        };

        self.hir
            .hir_items
            .insert(expr.id, StoredHirItem::Expr(expr.clone()));

        if let Some(fn_name) = fn_name {
            self.hir.expanded_by_builtins.insert(expr.id, fn_name);
        }
    }
}

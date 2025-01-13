use brim_ast::expr::{Expr, ExprKind, IfExpr};
use brim_ast::item::{Block, FnDecl, FnReturnType, FnSignature, Generics, Ident, Item, ItemKind};
use brim_ast::stmts::{Let, Stmt, StmtKind};
use brim_ast::ty::Ty;

pub trait AstWalker {
    fn visit_item(&mut self, item: &mut Item);
    fn visit_stmt(&mut self, stmt: &mut Stmt);
    fn visit_expr(&mut self, expr: &mut Expr);
    fn visit_let(&mut self, let_stmt: &mut Let);
    fn visit_block(&mut self, block: &mut Block);
    fn visit_ty(&mut self, ty: &mut Ty);
    fn visit_generics(&mut self, generics: &mut Generics);

    fn walk_item(&mut self, item: &mut Item) {
        match &mut item.kind {
            ItemKind::Fn(func) => {
                self.visit_generics(&mut func.generics);
                if let Some(body) = &mut func.body {
                    self.walk_block(body);
                }
                self.visit_signature(&mut func.sig);
            }
        }
    }

    fn visit_signature(&mut self, signature: &mut FnSignature) {
        for param in &mut signature.params {
            self.visit_ty(&mut param.ty);
        }

        if let FnReturnType::Ty(ref mut ty) = signature.return_type {
            self.visit_ty(ty);
        }
    }

    fn walk_stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.kind {
            StmtKind::Let(let_stmt) => self.visit_let(let_stmt),
            StmtKind::Item(item) => self.visit_item(item),
            StmtKind::Expr(expr) => self.visit_expr(expr),
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
            ExprKind::Var(_) => {}
            ExprKind::AssignOp(lhs, _, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::Assign(lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::If(if_expr) => {
                self.visit_expr(&mut if_expr.condition);

                for else_if in &mut if_expr.else_ifs {
                    self.visit_expr(&mut else_if.condition);
                    self.walk_expr(&mut else_if.block);
                }

                if let Some(else_branch) = &mut if_expr.else_block {
                    self.walk_expr(else_branch);
                }
            }
            ExprKind::Block(block) => self.walk_block(block),
            ExprKind::Call(func, args) => {
                self.visit_expr(func);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
        }
    }

    fn walk_let(&mut self, let_stmt: &mut Let) {
        if let Some(ty) = &mut let_stmt.ty {
            self.visit_ty(ty);
        }

        if let Some(value) = &mut let_stmt.value {
            self.visit_expr(value);
        }
    }

    fn walk_block(&mut self, block: &mut Block) {
        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
    }
}

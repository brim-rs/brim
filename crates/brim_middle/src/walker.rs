use brim_ast::{
    expr::{Expr, ExprKind, MatchArm},
    item::{
        Block, FnDecl, FnReturnType, FnSignature, GenericArgs, Generics, Ident, Item, ItemKind,
        Struct, TypeAlias, Use,
    },
    stmts::{Let, Stmt, StmtKind},
    ty::Ty,
};
use indexmap::IndexMap;

pub trait AstWalker {
    // Visit methods - customize behavior for each node type
    fn visit_item(&mut self, item: &mut Item) {
        self.walk_item(item);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        self.walk_stmt(stmt);
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        self.walk_expr(expr);
    }

    fn visit_let(&mut self, let_stmt: &mut Let) {
        self.walk_let(let_stmt);
    }

    fn visit_block(&mut self, block: &mut Block) {
        self.walk_block(block);
    }

    fn visit_ty(&mut self, _ty: &mut Ty) {}

    fn visit_generics(&mut self, _generics: &mut Generics) {}

    fn visit_use(&mut self, _use_stmt: &mut Use) {}

    fn visit_struct(&mut self, _str: &mut Struct) {}

    fn visit_type_alias(&mut self, _type_alias: &mut TypeAlias) {}

    fn visit_struct_constructor(
        &mut self,
        _ident: &mut Ident,
        _generic_args: &mut GenericArgs,
        _fields: &mut IndexMap<Ident, Expr>,
    ) {
    }

    fn visit_fn(&mut self, func: &mut FnDecl) {
        self.visit_generics(&mut func.generics);
        self.visit_signature(&mut func.sig);
        if let Some(body) = &mut func.body {
            self.visit_block(body);
        }
    }

    fn visit_signature(&mut self, signature: &mut FnSignature) {
        self.walk_signature(signature);
    }

    fn visit_builtin(&mut self, _name: &mut Ident, _args: &mut Vec<Expr>) {}

    // Walk methods - traverse child nodes
    fn walk_item(&mut self, item: &mut Item) {
        match &mut item.kind {
            ItemKind::Fn(func) => self.visit_fn(func),
            ItemKind::Use(use_stmt) => self.visit_use(use_stmt),
            ItemKind::Struct(str) => self.visit_struct(str),
            ItemKind::TypeAlias(type_alias) => self.visit_type_alias(type_alias),
            ItemKind::External(external) => {
                for item in &mut external.items {
                    match &mut item.kind {
                        ItemKind::Fn(func) => self.visit_fn(func),
                        ItemKind::TypeAlias(ty) => self.visit_type_alias(ty),
                        _ => unreachable!("not allowed"),
                    }
                }
            }
            ItemKind::Enum(en) => {}
            ItemKind::Namespace(_) | ItemKind::Module(_) => {}
        }
    }

    fn walk_stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.kind {
            StmtKind::Let(let_stmt) => self.visit_let(let_stmt),
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
                self.visit_expr(func);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Comptime(inner) => self.visit_expr(inner),
            ExprKind::Array(items) => {
                for item in items {
                    self.visit_expr(item);
                }
            }
            ExprKind::Builtin(ident, args) => self.visit_builtin(ident, args),
            ExprKind::StructConstructor(ident, gens, fields) => {
                self.visit_struct_constructor(ident, gens, fields)
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
            ExprKind::Path(_) => {}
            ExprKind::Type(ty) => {
                self.visit_ty(ty);
            }
            ExprKind::StaticAccess(ident, expr) => {
                self.visit_expr(expr);
            }
            ExprKind::MethodCall(ident, expr) => {
                self.visit_method_call(ident, expr);
            }
            ExprKind::Unwrap(expr) => {
                self.visit_expr(expr);
            }
        }
    }

    fn visit_method_call(&mut self, ident: &Vec<Ident>, args: &mut Box<Expr>) {
        self.visit_expr(args);
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

    fn walk_signature(&mut self, signature: &mut FnSignature) {
        for param in &mut signature.params {
            self.visit_ty(&mut param.ty);
        }
        if let FnReturnType::Ty(ref mut ty) = signature.return_type {
            self.visit_ty(ty);
        }
    }
}

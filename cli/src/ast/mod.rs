pub mod statements;
pub mod expressions;
pub mod types;
pub mod item;

use indexmap::IndexMap;
use crate::ast::expressions::{AccessExpr, AccessKind, AnonymousFunction, ArrayExpr, Assign, AssignOperator, BinOperator, Binary, CallExpr, Expr, ExprKind, Literal, LiteralType, ObjectExpr, Parenthesized, StructConstructor, UnOperator, Unary, Variable};
use crate::ast::item::TopLevelItem;
use crate::ast::statements::{Block, Const, ElseBlock, FnParam, Function, Let, Loop, Return, Stmt, StmtKind, Struct, StructField, StructImpl, TraitDef, TraitImpl, Try, TypeAnnotation, Use, While};
use crate::ast::types::TypeKind;
use crate::error::span::TextSpan;
use crate::idx::Idx;
use crate::idx;
use crate::idx::IdxVec;
use crate::lexer::tokens::Token;

idx!(StmtId);
idx!(ExprId);
idx!(ItemId);

#[derive(Debug, Clone)]
pub struct Ast {
    pub statements: IdxVec<StmtId, Stmt>,
    pub expressions: IdxVec<ExprId, Expr>,
    // We need to store top-level items separately so we can know how to interpret AST
    pub top_level_items: IdxVec<ItemId, TopLevelItem>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            statements: IdxVec::new(),
            expressions: IdxVec::new(),
            top_level_items: IdxVec::new(),
        }
    }

    pub fn query_expr(&self, expr_id: ExprId) -> &Expr {
        &self.expressions[expr_id]
    }

    fn query_expr_mut(&mut self, expr_id: ExprId) -> &mut Expr {
        &mut self.expressions[expr_id]
    }

    pub fn query_stmt(&self, stmt_id: StmtId) -> &Stmt {
        &self.statements[stmt_id]
    }

    fn query_stmt_mut(&mut self, stmt_id: StmtId) -> &mut Stmt {
        &mut self.statements[stmt_id]
    }

    pub fn new_expr(&mut self, kind: ExprKind) -> &Expr {
        let expr = Expr::new(kind, ExprId::new(0), TypeKind::Undefined);
        let id = self.expressions.push(expr);
        self.expressions[id].id = id;
        &self.expressions[id]
    }

    pub fn new_stmt(&mut self, kind: StmtKind) -> &Stmt {
        let stmt = Stmt::new(kind, StmtId::new(0));
        let id = self.statements.push(stmt);
        self.statements[id].id = id;
        &self.statements[id]
    }

    pub fn new_item(&mut self, stmt: StmtId) -> &TopLevelItem {
        let item = TopLevelItem::new(stmt, ItemId::new(0));
        let id = self.top_level_items.push(item);
        self.top_level_items[id].id = id;
        &self.top_level_items[id]
    }

    pub fn query_item(&self, item_id: ItemId) -> &TopLevelItem {
        &self.top_level_items[item_id]
    }
}

impl Ast {
    pub fn new_assignment(&mut self, left: ExprId, op: AssignOperator, right: ExprId) -> ExprId {
        let expr = self.new_expr(ExprKind::Assign(Assign { left, op, right }));
        expr.id
    }

    pub fn new_unary(&mut self, operator: UnOperator, operand: ExprId, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Unary(Unary { operator, expr: operand, token }));
        expr.id
    }

    pub fn new_binary(&mut self, left: ExprId, operator: BinOperator, right: ExprId) -> ExprId {
        let expr = self.new_expr(ExprKind::Binary(Binary { left, right, operator: operator.kind }));

        expr.id
    }

    pub fn new_variable(&mut self, token: Token, ident: String) -> ExprId {
        let expr = self.new_expr(ExprKind::Variable(Variable {
            token,
            ident,
        }));
        expr.id
    }

    pub fn new_call(&mut self, callee: String, args: Vec<ExprId>, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Call(CallExpr {
            callee,
            args,
            token,
        }));
        expr.id
    }

    pub fn new_field_access(&mut self, base: ExprId, field: ExprId, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Access(AccessExpr {
            base,
            access: AccessKind::Field(field),
            token,
        }));
        expr.id
    }

    pub fn new_index_access(&mut self, base: ExprId, index: ExprId, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Access(AccessExpr {
            base,
            access: AccessKind::Index(index),
            token,
        }));
        expr.id
    }

    pub fn new_static_method_access(&mut self, base: ExprId, method: ExprId, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Access(AccessExpr {
            base,
            access: AccessKind::StaticMethod(method),
            token,
        }));
        expr.id
    }

    pub fn new_struct_constructor(&mut self, name: String, fields: IndexMap<String, ExprId>, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::StructConstructor(StructConstructor {
            name,
            fields,
            token,
        }));
        expr.id
    }

    pub fn new_integer(&mut self, value: i64, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Literal(Literal {
            value: LiteralType::Int(value),
            token,
        }));
        expr.id
    }

    pub fn new_float(&mut self, value: f64, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Literal(Literal {
            value: LiteralType::Float(value),
            token,
        }));
        expr.id
    }

    pub fn new_null(&mut self, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Literal(Literal {
            value: LiteralType::Null,
            token,
        }));
        expr.id
    }

    pub fn new_boolean(&mut self, value: bool, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Literal(Literal {
            value: LiteralType::Bool(value),
            token,
        }));
        expr.id
    }

    pub fn new_object(&mut self, fields: IndexMap<String, ExprId>, braces: (Token, Token)) -> ExprId {
        let expr = self.new_expr(ExprKind::Object(
            ObjectExpr {
                fields,
                braces,
            }
        ));
        expr.id
    }

    pub fn new_parenthesized(&mut self, expr: ExprId) -> ExprId {
        let expr = self.new_expr(ExprKind::Parenthesized(
            Parenthesized {
                expr,
            }
        ));
        expr.id
    }

    pub fn new_char(&mut self, value: char, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Literal(Literal {
            value: LiteralType::Char(value),
            token,
        }));
        expr.id
    }

    pub fn new_string(&mut self, value: String, token: Token) -> ExprId {
        let expr = self.new_expr(ExprKind::Literal(Literal {
            value: LiteralType::String(value),
            token,
        }));
        expr.id
    }

    pub fn new_array(&mut self, elements: Vec<ExprId>) -> ExprId {
        let expr = self.new_expr(ExprKind::Array(
            ArrayExpr {
                exprs: elements,
            }
        ));
        expr.id
    }

    pub fn new_expr_stmt(&mut self, expr: ExprId) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Expr(expr));
        stmt.id
    }

    pub fn new_break_stmt(&mut self, token: Token) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Break(token));
        stmt.id
    }

    pub fn new_continue_stmt(&mut self, token: Token) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Continue(token));
        stmt.id
    }

    pub fn new_loop(&mut self, body: StmtId, token: Token) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Loop(Loop {
            block: body,
            token,
        }));
        stmt.id
    }

    pub fn new_block(&mut self, stmts: Vec<StmtId>) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Block(
            Block {
                stmts
            }
        ));
        stmt.id
    }

    pub fn new_fn(&mut self, fn_token: Token, name: Token, params: Vec<FnParam>, body: Option<StmtId>, public: bool, return_type: Option<TypeAnnotation>, is_static: bool) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Fn(
            Function {
                fn_token,
                name,
                params,
                body,
                public,
                return_type,
                is_static,
            }
        ));
        stmt.id
    }

    pub fn new_use(&mut self, use_token: Token, from: Token, items: Vec<Token>) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Use(
            Use {
                use_token,
                from,
                items,
            }
        ));
        stmt.id
    }

    pub fn new_if(&mut self, if_token: Token, condition: ExprId, then_block: StmtId, else_ifs: Vec<ElseBlock>, else_block: Option<ElseBlock>) -> StmtId {
        let stmt = self.new_stmt(StmtKind::If(
            statements::If {
                if_token,
                condition,
                then_block,
                else_ifs,
                else_block,
            }
        ));
        stmt.id
    }

    pub fn new_let(&mut self, ident: Token, initializer: ExprId, type_annotation: Option<TypeAnnotation>) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Let(
            Let {
                ident,
                initializer,
                type_annotation,
            }
        ));
        stmt.id
    }

    pub fn new_return(&mut self, return_token: Token, expr: Option<ExprId>) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Return(
            Return {
                return_token,
                expr,
            }
        ));
        stmt.id
    }

    pub fn new_try(&mut self, token: Token, expr: ExprId) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Try(
            Try {
                token,
                expr,
            }
        ));

        stmt.id
    }

    pub fn new_while(&mut self, while_token: Token, condition: ExprId, block: StmtId) -> StmtId {
        let stmt = self.new_stmt(StmtKind::While(
            While {
                while_token,
                condition,
                block,
            }
        ));
        stmt.id
    }

    pub fn new_struct(&mut self, struct_token: Token, name: Token, fields: IndexMap<String, StructField>, public: bool) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Struct(
            Struct {
                struct_token,
                name,
                fields,
                public,
                impls: vec![],
                trait_impls: vec![],
            }
        ));
        stmt.id
    }

    pub fn new_const(&mut self, expr: ExprId, ident: Token, public: bool) -> StmtId {
        let stmt = self.new_stmt(StmtKind::Const(
            Const {
                expr,
                ident,
                public,
            }
        ));
        stmt.id
    }

    pub fn new_trait_def(&mut self, trait_token: Token, name: Token, methods: Vec<StmtId>, public: bool) -> StmtId {
        let stmt = self.new_stmt(StmtKind::TraitDef(
            TraitDef {
                trait_token,
                name,
                methods,
                public,
            }
        ));
        stmt.id
    }

    pub fn new_trait_impl(&mut self, impl_token: Token, trait_name: Token, for_token: Token, struct_name: Token, methods: Vec<StmtId>) -> StmtId {
        let stmt = self.new_stmt(StmtKind::TraitImpl(
            TraitImpl {
                impl_token,
                trait_name,
                for_token,
                struct_name,
                methods,
            }
        ));
        stmt.id
    }

    pub fn new_struct_impl(&mut self, impl_token: Token, struct_name: Token, methods: Vec<StmtId>) -> StmtId {
        let stmt = self.new_stmt(StmtKind::StructImpl(
            StructImpl {
                impl_token,
                struct_name,
                methods,
            }
        ));
        stmt.id
    }

    pub fn new_anonymous_function(&mut self, params: Vec<FnParam>, body: StmtId, return_type: Option<TypeAnnotation>, pipes: (Token, Token)) -> ExprId {
        let expr = self.new_expr(ExprKind::AnonymousFunction(
            AnonymousFunction {
                params,
                body,
                pipes,
                return_type,
            }
        ));
        expr.id
    }
}

pub trait GetSpan {
    fn span(&self, ast: &Ast) -> TextSpan;
}
use crate::{
    HirId,
    expr::HirExpr,
    items::{HirFn, HirFnSig, HirGenericParam, HirGenerics, HirItem, HirItemKind, HirParam},
    stmts::HirStmt,
    ty::HirTy,
};
use brim_ast::{
    NodeId,
    item::{Item, ItemKind},
};
use brim_ctx::{
    ModuleId,
    modules::{Module, ModuleMap},
};
use std::{collections::HashMap, path::PathBuf};
use brim_ast::expr::{BinOpKind, ConstExpr, Expr, ExprKind};
use brim_ast::item::{FnReturnType, GenericKind};
use brim_ast::token::AssignOpToken;
use crate::expr::{HirConstExpr, HirExprKind};
use crate::items::HirGenericKind;
use crate::ty::HirTyKind;

#[derive(Clone, Debug)]
pub struct LocId {
    pub id: HirId,
    pub module: ModuleId,
}

pub fn transform_module(map: ModuleMap) -> HirModuleMap {
    let mut transformer = Transformer::new();
    transformer.transform_modules(map)
}

#[derive(Debug, Clone)]
pub struct HirModuleMap {
    pub modules: Vec<HirModule>,
    pub hir_items: HashMap<HirId, StoredHirItem>,
}

impl HirModuleMap {
    pub fn new() -> Self {
        Self {
            modules: vec![],
            hir_items: Default::default(),
        }
    }

    pub fn insert_hir_item(&mut self, id: HirId, item: StoredHirItem) {
        self.hir_items.insert(id, item);
    }

    pub fn insert_hir_expr(&mut self, id: HirId, expr: HirExpr) {
        self.hir_items.insert(id, StoredHirItem::Expr(expr));
    }

    pub fn insert_hir_stmt(&mut self, id: HirId, stmt: HirStmt) {
        self.hir_items.insert(id, StoredHirItem::Stmt(stmt));
    }

    pub fn new_module(&mut self, module: HirModule) {
        self.modules.push(module);
    }
}

#[derive(Clone, Debug)]
pub enum StoredHirItem {
    Item(HirItem),
    Stmt(HirStmt),
    Expr(HirExpr),
}

#[derive(Clone, Debug)]
pub struct HirModule {
    /// In hir we no longer use file ids, we use module ids.
    pub mod_id: ModuleId,
    pub items: Vec<HirItem>,
    // Not sure if this will be needed
    path: PathBuf,
}

#[derive(Debug)]
pub struct Transformer {
    pub map: HirModuleMap,
    pub last_id: usize,
}

impl Transformer {
    pub fn new() -> Self {
        Self { map: HirModuleMap::new(), last_id: 0 }
    }

    pub fn transform_modules(&mut self, module: ModuleMap) -> HirModuleMap {
        for module in module.modules {
            let hir_module = self.transform_module(module);
            self.map.new_module(hir_module);
        }

        self.map.clone()
    }

    pub fn transform_module(&mut self, module: Module) -> HirModule {
        let items = module
            .barrel
            .items
            .iter()
            .map(|item| self.transform_item(item.clone()))
            .collect();
        HirModule {
            mod_id: ModuleId::from_usize(module.barrel.file_id),
            items,
            path: module.path,
        }
    }

    pub fn transform_item(&mut self, item: Item) -> HirItem {
        let hir_item_kind = match item.kind.clone() {
            ItemKind::Fn(f_decl) => HirItemKind::Fn(HirFn {
                sig: HirFnSig {
                    constant: f_decl.sig.constant.as_bool(),
                    name: f_decl.sig.name,
                    return_type: if let FnReturnType::Ty(ty) = f_decl.sig.return_type {
                        Some(self.transform_ty(ty))
                    } else {
                        None
                    },
                    params: f_decl
                        .sig
                        .params
                        .iter()
                        .map(|param| HirParam {
                            id: HirId::from_u32(param.id.as_u32()),
                            span: param.span,
                            name: param.name,
                            ty: self.transform_ty(param.ty.clone()),
                        })
                        .collect(),
                    generics: HirGenerics {
                        params: f_decl
                            .generics
                            .params
                            .iter()
                            .map(|param| HirGenericParam {
                                id: HirId::from_u32(param.id.as_u32()),
                                name: param.ident,
                                kind: self.hir_generic_kind(param.kind.clone()),
                            })
                            .collect(),
                        span: f_decl.generics.span,
                    },
                    span: f_decl.sig.span,
                },
            }),
            _ => {}
        };

        let item = HirItem {
            id: self.hir_id(),
            span: item.span,
            ident: item.ident,
            kind: hir_item_kind,
            is_public: item.vis.kind.is_public(),
        };

        self.map.insert_hir_item(item.id, StoredHirItem::Item(item.clone()));
    }

    pub fn hir_generic_kind(&mut self, kind: GenericKind) -> HirGenericKind {
        match kind {
            GenericKind::Type { default } => HirGenericKind::Type {
                default: default.map(|ty| self.transform_ty(ty)),
            },
            GenericKind::NonType { ty, default } => HirGenericKind::Const {
                ty: self.transform_ty(ty),
                default: default.map(|expr| self.transform_const_expr(expr)),
            },
        }
    }

    pub fn transform_expr(&mut self, expr: Expr) -> (HirExpr, HirId) {
        let expr = HirExpr {
            id: self.hir_id(),
            span: expr.span,
            kind: match expr.kind {
                ExprKind::Binary(lhs, op, rhs) => HirExprKind::Binary(
                    Box::new(self.transform_expr(*lhs).0),
                    op,
                    Box::new(self.transform_expr(*rhs).0),
                ),
                ExprKind::Unary(op, expr) => HirExprKind::Unary(op, Box::new(self.transform_expr(*expr).0)),
                ExprKind::Field(expr, ident) => HirExprKind::Field(Box::new(self.transform_expr(*expr).0), ident),
                ExprKind::Index(expr, index) => HirExprKind::Index(Box::new(self.transform_expr(*expr).0), Box::new(self.transform_expr(*index).0)),
                ExprKind::Literal(lit) => HirExprKind::Literal(lit),
                ExprKind::Paren(expr) => self.transform_expr(*expr).0.kind,
                ExprKind::Return(expr) => HirExprKind::Return(Box::new(self.transform_expr(*expr).0)),
                ExprKind::Var(ident) => HirExprKind::Var(ident),
                // Ops like += turn into x = x + 1
                ExprKind::AssignOp(lhs, op, rhs) => {
                    let bin_op = match op {
                        AssignOpToken::PlusEq => BinOpKind::Plus,
                        AssignOpToken::MinusEq => BinOpKind::Minus,
                        AssignOpToken::StarEq => BinOpKind::Multiply,
                        AssignOpToken::SlashEq => BinOpKind::Divide,
                        AssignOpToken::ModEq => BinOpKind::Modulo,
                        AssignOpToken::AndEq => BinOpKind::And,
                        AssignOpToken::OrEq => BinOpKind::Or,
                        AssignOpToken::CaretEq => BinOpKind::Caret,
                        AssignOpToken::ShlEq => BinOpKind::ShiftLeft,
                        AssignOpToken::ShrEq => BinOpKind::ShiftRight,
                    };

                    HirExprKind::Assign(
                        Box::new(self.transform_expr(*lhs.clone()).0),
                        Box::new(HirExpr {
                            id: self.hir_id(),
                            span: expr.span,
                            kind: HirExprKind::Binary(
                                Box::new(self.transform_expr(*lhs).0),
                                bin_op,
                                Box::new(self.transform_expr(*rhs).0),
                            ),
                            ty: HirTyKind::Placeholder,
                        }),
                    )
                }
                ExprKind::Assign(lhs, rhs) => HirExprKind::Assign(Box::new(self.transform_expr(*lhs).0), Box::new(self.transform_expr(*rhs).0)),
                ExprKind::If(if_expr) => self.transform_if_expr(if_expr),
                ExprKind::Block(block) => HirExprKind::Block(self.transform_block(block)),
                ExprKind::Call(expr, args) => HirExprKind::Call(Box::new(self.transform_expr(*expr).0), args.iter().map(|arg| self.transform_expr(arg.clone()).0).collect()),
            },
            ty: HirTyKind::Placeholder,
        };
        self.map.insert_hir_expr(expr.id, expr.clone());

        (expr.clone(), expr.id)
    }

    pub fn transform_const_expr(&mut self, expr: ConstExpr) -> HirConstExpr {
        let (_, id) = self.transform_expr(*expr.expr.clone());

        HirConstExpr {
            id: self.hir_id(),
            span: expr.expr.span,
            body: id,
        }
    }

    pub fn hir_id(&mut self) -> HirId {
        self.last_id += 1;

        HirId::from_usize(self.last_id)
    }
}

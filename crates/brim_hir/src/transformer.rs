use crate::{
    HirId,
    expr::{HirBlock, HirConstExpr, HirExpr, HirExprKind},
    items::{
        HirFn, HirFnSig, HirGenericKind, HirGenericParam, HirGenerics, HirImportsKind, HirItem,
        HirItemKind, HirParam, HirUse,
    },
    stmts::{HirStmt, HirStmtKind},
    ty::{HirTy, HirTyKind},
};
use brim_ast::{
    expr::{BinOpKind, ConstExpr, Expr, ExprKind},
    item::{Block, FnReturnType, GenericKind, ImportsKind, Item, ItemKind},
    stmts::{Stmt, StmtKind},
    token::AssignOpToken,
    ty::TyKind,
};
use brim_ctx::{
    GlobalSymbolId, ModuleId,
    modules::{Module, ModuleMap},
};
use std::{collections::HashMap, path::PathBuf};

#[derive(Clone, Debug)]
pub struct LocId {
    pub id: HirId,
    pub module: ModuleId,
}

pub fn transform_module(map: ModuleMap) -> HirModuleMap {
    let mut transformer = Transformer::new(map);
    transformer.transform_modules()
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
    pub module_map: ModuleMap,
    current_mod_id: ModuleId,
}

impl Transformer {
    pub fn new(module_map: ModuleMap) -> Self {
        Self {
            map: HirModuleMap::new(),
            last_id: 0,
            module_map,
            current_mod_id: ModuleId::from_usize(0),
        }
    }

    pub fn transform_modules(&mut self) -> HirModuleMap {
        for module in self.module_map.modules.clone() {
            self.current_mod_id = ModuleId::from_usize(module.barrel.file_id);
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
            ItemKind::Fn(f_decl) => {
                let body = if let Some(body) = f_decl.body {
                    let hir = HirExpr {
                        kind: HirExprKind::Block(self.transform_block(body.clone())),
                        span: body.span,
                        ty: HirTyKind::Placeholder,
                        id: self.hir_id(),
                    };

                    self.map.insert_hir_expr(hir.id, hir.clone());
                    Some(hir.id)
                } else {
                    None
                };

                HirItemKind::Fn(HirFn {
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
                    body,
                })
            }
            ItemKind::Use(u) => {
                let imports = match u.imports {
                    ImportsKind::List(list) => HirImportsKind::List(list),
                    ImportsKind::All => HirImportsKind::All,
                };

                HirItemKind::Use(HirUse {
                    span: u.span,
                    imports,
                    resolved_path: self
                        .module_map
                        .imports
                        .get(&GlobalSymbolId {
                            item_id: item.id,
                            mod_id: self.current_mod_id,
                        })
                        .unwrap()
                        .clone(),
                })
            }
        };

        let item = HirItem {
            id: self.hir_id(),
            span: item.span,
            ident: item.ident,
            kind: hir_item_kind,
            is_public: item.vis.kind.is_public(),
        };

        self.map
            .insert_hir_item(item.id, StoredHirItem::Item(item.clone()));

        item
    }

    pub fn transform_block(&mut self, block: Block) -> HirBlock {
        let stmts = block
            .stmts
            .iter()
            .map(|stmt| self.transform_stmt(stmt.clone()))
            .collect();

        HirBlock {
            id: self.hir_id(),
            span: block.span,
            stmts,
        }
    }

    pub fn transform_stmt(&mut self, stmt: Stmt) -> HirStmt {
        HirStmt {
            id: self.hir_id(),
            span: stmt.span,
            kind: match stmt.kind {
                StmtKind::Expr(expr) => HirStmtKind::Expr(self.transform_expr(expr).0),
                StmtKind::Let(le) => HirStmtKind::Let {
                    ty: le.ty.map(|ty| self.transform_ty(ty)),
                    ident: le.ident,
                    value: le.value.map(|expr| self.transform_expr(expr).0),
                },
            },
        }
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
                ExprKind::Unary(op, expr) => {
                    HirExprKind::Unary(op, Box::new(self.transform_expr(*expr).0))
                }
                ExprKind::Field(expr, ident) => {
                    HirExprKind::Field(Box::new(self.transform_expr(*expr).0), ident)
                }
                ExprKind::Index(expr, index) => HirExprKind::Index(
                    Box::new(self.transform_expr(*expr).0),
                    Box::new(self.transform_expr(*index).0),
                ),
                ExprKind::Literal(lit) => HirExprKind::Literal(lit),
                ExprKind::Paren(expr) => self.transform_expr(*expr).0.kind,
                ExprKind::Return(expr) => {
                    HirExprKind::Return(Box::new(self.transform_expr(*expr).0))
                }
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
                ExprKind::Assign(lhs, rhs) => HirExprKind::Assign(
                    Box::new(self.transform_expr(*lhs).0),
                    Box::new(self.transform_expr(*rhs).0),
                ),
                // ExprKind::If(if_expr) => self.transform_if_expr(if_expr),
                ExprKind::Block(block) => HirExprKind::Block(self.transform_block(block)),
                ExprKind::Call(expr, args) => HirExprKind::Call(
                    Box::new(self.transform_expr(*expr).0),
                    args.iter()
                        .map(|arg| self.transform_expr(arg.clone()).0)
                        .collect(),
                ),
                _ => todo!(),
            },
            ty: HirTyKind::Placeholder,
        };
        self.map.insert_hir_expr(expr.id, expr.clone());

        (expr.clone(), expr.id)
    }

    pub fn transform_ty(&mut self, ty: brim_ast::ty::Ty) -> HirTy {
        HirTy {
            id: self.hir_id(),
            span: ty.span,
            kind: match ty.kind {
                TyKind::Ptr(ty, cnst) => HirTyKind::Ptr(Box::new(self.transform_ty(*ty)), cnst),
                TyKind::Array(ty, len) => HirTyKind::Array(
                    Box::new(self.transform_ty(*ty)),
                    self.transform_const_expr(len),
                ),
                TyKind::Ref(ty, cnst) => HirTyKind::Ref(Box::new(self.transform_ty(*ty)), cnst),
                TyKind::Primitive(primitive) => HirTyKind::Primitive(primitive),
                TyKind::Vec(ty) => HirTyKind::Vec(Box::new(self.transform_ty(*ty))),
                TyKind::Const(ty) => HirTyKind::Const(Box::new(self.transform_ty(*ty))),
                TyKind::Ident { ident, generics } => HirTyKind::Ident {
                    ident,
                    generics: HirGenerics {
                        params: generics
                            .params
                            .iter()
                            .map(|param| HirGenericParam {
                                id: HirId::from_u32(param.id.as_u32()),
                                name: param.ident,
                                kind: self.hir_generic_kind(param.kind.clone()),
                            })
                            .collect(),
                        span: generics.span,
                    },
                },
                TyKind::Err(e) => HirTyKind::Err(e),
            },
        }
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

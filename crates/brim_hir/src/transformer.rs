use crate::{
    CompiledModules,
    builtin::get_builtin_function,
    comptime::ComptimeReturnValue,
    expr::{
        ComptimeValue, HirBlock, HirConditionBranch, HirExpr, HirExprKind, HirIfStmt, HirMatch,
        HirMatchArm, HirStructConstructor,
    },
    items::{
        HirAttribute, HirEnum, HirEnumField, HirEnumVariant, HirExternBlock, HirField, HirFn,
        HirFnParams, HirFnSig, HirGenericArg, HirGenericArgs, HirGenericKind, HirGenericParam,
        HirGenerics, HirImportsKind, HirItem, HirItemKind, HirParam, HirStruct, HirTypeAlias,
        HirUse,
    },
    stmts::{HirStmt, HirStmtKind},
    ty::{HirTy, HirTyKind},
};
use brim_ast::{
    ItemId,
    expr::{BinOpKind, Expr, ExprKind, Match, MatchArm},
    item::{
        Block, Enum, FnDecl, FnReturnType, GenericArgs, GenericKind, Generics, Ident, ImportsKind,
        Item, ItemKind, Struct, TypeAlias, TypeAliasValue,
    },
    stmts::{IfStmt, Stmt, StmtKind},
    token::{AssignOpToken, Lit, LitKind},
    ty::{PrimitiveType, Ty, TyKind},
};
use brim_diagnostics::diagnostic::ToDiagnostic;
use brim_middle::{
    GlobalSymbol, ModuleId, SymbolTable,
    barrel::Barrel,
    modules::{Module, ModuleMap},
    temp_diag::TemporaryDiagnosticContext,
};
use brim_span::{span::Span, symbols::Symbol};
use std::{collections::HashMap, path::PathBuf, vec};

#[derive(Clone, Debug)]
pub struct LocId {
    pub id: ItemId,
    pub module: ModuleId,
}

pub fn transform_module(
    map: ModuleMap,
    compiled_modules: &mut CompiledModules,
) -> (HirModuleMap, TemporaryDiagnosticContext) {
    let mut transformer = Transformer::new(map, compiled_modules);
    (transformer.transform_modules(), transformer.ctx)
}

#[derive(Debug, Clone)]
pub struct HirModuleMap {
    pub modules: Vec<HirModule>,
    pub hir_items: HashMap<ItemId, StoredHirItem>,
    pub expanded_by_builtins: HashMap<ItemId, String>,
    pub builtin_args: HashMap<ItemId, Vec<HirExpr>>,
    pub symbols: SymbolTable,
}

impl HirModuleMap {
    pub fn new() -> Self {
        Self {
            modules: Vec::new(),
            hir_items: HashMap::new(),
            expanded_by_builtins: HashMap::new(),
            symbols: SymbolTable::new(),
            builtin_args: HashMap::new(),
        }
    }

    pub fn insert_hir_item(&mut self, id: ItemId, item: StoredHirItem) {
        self.hir_items.insert(id, item);
    }

    pub fn insert_hir_expr(&mut self, id: ItemId, expr: HirExpr) {
        self.hir_items.insert(id, StoredHirItem::Expr(expr));
    }

    pub fn new_module(&mut self, module: HirModule) {
        self.modules.push(module);
    }

    pub fn get(&self, id: ItemId) -> Option<&StoredHirItem> {
        self.hir_items.get(&id)
    }

    pub fn get_mut(&mut self, id: ItemId) -> Option<&mut StoredHirItem> {
        self.hir_items.get_mut(&id)
    }

    pub fn get_expr(&self, id: ItemId) -> &HirExpr {
        match self.get(id) {
            Some(StoredHirItem::Expr(expr)) => expr,
            _ => panic!("Expected expr for ID {:?}, but found {:?}", id, self.get(id)),
        }
    }

    pub fn get_expr_mut(&mut self, id: ItemId) -> &mut HirExpr {
        match self.get_mut(id) {
            Some(StoredHirItem::Expr(expr)) => expr,
            _ => panic!("Expected expr for ID {id:?}"),
        }
    }

    pub fn get_module(&self, id: ModuleId) -> Option<&HirModule> {
        self.modules.iter().find(|module| module.mod_id == id)
    }

    pub fn get_fn(&self, id: ModuleId, name: &str) -> Option<&HirFn> {
        self.get_module(id).and_then(|module| {
            module.items.iter().find_map(|item| match self.get(*item) {
                Some(StoredHirItem::Item(HirItem { kind: HirItemKind::Fn(f), .. }))
                    if f.sig.name.to_string() == *name =>
                {
                    Some(f)
                }
                _ => None,
            })
        })
    }

    pub fn get_module_by_id(&self, id: ModuleId) -> Option<&HirModule> {
        self.modules.iter().find(|module| module.mod_id == id)
    }

    /// looks for an expr in a self.builtin_args and updates if found
    pub fn update_builtins(&mut self, expr: HirExpr) {
        for args in self.builtin_args.values_mut() {
            for arg in args.iter_mut() {
                if arg.id == expr.id {
                    *arg = expr.clone();
                }
            }
        }
    }
}

impl Default for HirModuleMap {
    fn default() -> Self {
        Self::new()
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
    pub items: Vec<ItemId>,
    // Not sure if this will be needed
    pub path: PathBuf,
    pub imports: Vec<LocId>,
    pub barrel: Barrel,
}

#[derive(Debug)]
pub struct Transformer<'a> {
    pub map: HirModuleMap,
    pub last_id: usize,
    pub module_map: ModuleMap,
    pub current_mod_id: ModuleId,
    pub ctx: TemporaryDiagnosticContext,
    pub compiled: &'a mut CompiledModules,
}

impl<'a> Transformer<'a> {
    pub fn new(module_map: ModuleMap, compiled: &'a mut CompiledModules) -> Self {
        Self {
            map: HirModuleMap::new(),
            last_id: 0,
            module_map,
            current_mod_id: ModuleId::from_usize(0),
            ctx: TemporaryDiagnosticContext::new(),
            compiled,
        }
    }

    pub fn transform_modules(&mut self) -> HirModuleMap {
        for module in self.module_map.modules.clone() {
            self.current_mod_id = ModuleId::from_usize(module.barrel.file_id);
            let module = self.transform_module(module);
            self.map.new_module(module);
        }

        for (module, symbols) in self.compiled.symbols.symbols.clone() {
            for sym in symbols {
                self.map.symbols.add_symbol(module, sym);
            }
        }

        self.map.clone()
    }

    pub fn transform_module(&mut self, module: Module) -> HirModule {
        let items: Vec<ItemId> = module
            .barrel
            .items
            .iter()
            .filter_map(|item| self.transform_item(item.clone()))
            .collect();

        HirModule {
            mod_id: ModuleId::from_usize(module.barrel.file_id),
            items,
            path: module.path,
            imports: vec![],
            barrel: module.barrel,
        }
    }

    pub fn transform_item(&mut self, item: Item) -> Option<ItemId> {
        let hir_item_kind = match item.kind.clone() {
            ItemKind::Fn(f_decl) => HirItemKind::Fn(self.transform_fn(f_decl)),
            ItemKind::Use(u) => {
                let imports = match u.imports {
                    ImportsKind::List(list, ..) => HirImportsKind::List(list),
                    ImportsKind::All(_) => HirImportsKind::All,
                    ImportsKind::Default(ident) => HirImportsKind::Default(ident),
                };

                HirItemKind::Use(HirUse {
                    span: u.span,
                    imports,
                    resolved_path: u.resolved.expect("path should be already resolved"),
                })
            }
            ItemKind::Struct(struc) => HirItemKind::Struct(self.transform_struct(struc)),
            ItemKind::TypeAlias(type_alias) => {
                HirItemKind::TypeAlias(self.transform_type_alias(type_alias))
            }
            ItemKind::Module(_) => return None,
            ItemKind::External(external) => {
                let items = external
                    .items
                    .iter()
                    .filter_map(|item| self.transform_item(item.clone()))
                    .collect::<Vec<_>>();

                HirItemKind::External(HirExternBlock {
                    abi: external.abi,
                    items,
                    span: external.span,
                })
            }
            ItemKind::Namespace(syms) => {
                let mut hash = HashMap::new();

                for sym in syms {
                    hash.insert(sym.0, GlobalSymbol::from_temp(sym.1));
                }

                HirItemKind::Namespace(hash)
            }
            ItemKind::Enum(en) => HirItemKind::Enum(self.transform_enum(en)),
        };

        let item = HirItem {
            id: item.id,
            span: item.span,
            ident: item.ident,
            kind: hir_item_kind,
            is_public: item.vis.kind.is_public(),
            mod_id: self.current_mod_id,
            attrs: item
                .attrs
                .iter()
                .map(|attr| HirAttribute {
                    name: attr.name,
                    args: attr.args.iter().map(|arg| self.transform_expr(arg.clone()).0).collect(),
                    span: attr.span,
                })
                .collect(),
        };

        self.map.insert_hir_item(item.id, StoredHirItem::Item(item.clone()));
        self.compiled.insert_item(item.clone());

        Some(item.id)
    }

    pub fn transform_type_alias(&mut self, type_alias: TypeAlias) -> HirTypeAlias {
        HirTypeAlias {
            span: type_alias.span,
            ident: type_alias.ident,
            ty: if let TypeAliasValue::Ty(ty) = type_alias.ty {
                ComptimeValue::Resolved(ComptimeReturnValue::Ty(self.transform_ty(ty).kind))
            } else if let TypeAliasValue::Const(expr) = type_alias.ty {
                ComptimeValue::Expr(Box::new(self.transform_expr(expr).0))
            } else {
                unreachable!()
            },
            generics: self.transform_generics(type_alias.generics),
        }
    }

    pub fn transform_fn(&mut self, f_decl: FnDecl) -> HirFn {
        let body = if let Some(body) = f_decl.body {
            let hir = HirExpr {
                kind: HirExprKind::Block(self.transform_block(body.clone())),
                span: body.span,
                ty: HirTyKind::Placeholder,
                id: body.id,
            };

            self.map.insert_hir_expr(hir.id, hir.clone());
            Some(hir.id)
        } else {
            None
        };

        let params = f_decl
            .sig
            .params
            .iter()
            .map(|param| HirParam {
                id: param.id,
                span: param.span,
                name: param.name,
                ty: self.transform_ty(param.ty.clone()),
            })
            .collect::<Vec<_>>();

        HirFn {
            sig: HirFnSig {
                constant: f_decl.sig.constant.is_some(),
                name: f_decl.sig.name,
                return_type: if let FnReturnType::Ty(ty) = f_decl.sig.return_type {
                    self.transform_ty(ty).kind
                } else {
                    HirTyKind::Primitive(PrimitiveType::Void)
                },
                params: HirFnParams {
                    span: if params.is_empty() {
                        Span::DUMMY
                    } else {
                        params[0].span.to(params[params.len() - 1].span)
                    },
                    params,
                },
                generics: self.transform_generics(f_decl.generics),
                span: f_decl.sig.span,
            },
            body,
            ctx: f_decl.context,
        }
    }

    pub fn transform_struct(&mut self, struc: Struct) -> HirStruct {
        let mut items = HashMap::new();

        for i in struc.items {
            if let Some(item) = self.transform_item(i.clone()) {
                items.insert(i.ident, item);
            }
        }

        HirStruct {
            ident: struc.ident,
            fields: struc
                .fields
                .iter()
                .map(|field| HirField {
                    id: field.id,
                    span: field.span,
                    ident: field.ident,
                    ty: self.transform_ty(field.ty.clone()).kind,
                    vis: field.vis.clone(),
                })
                .collect(),
            generics: self.transform_generics(struc.generics),
            span: struc.span,
            items,
        }
    }

    pub fn transform_enum(&mut self, enm: Enum) -> HirEnum {
        let mut items = HashMap::new();

        for i in enm.items {
            if let Some(item) = self.transform_item(i.clone()) {
                items.insert(i.ident, item);
            }
        }
        let mut variants = vec![];

        for variant in &enm.variants {
            let variant = HirEnumVariant {
                id: variant.id,
                span: variant.span,
                ident: variant.ident,
                fields: variant
                    .fields
                    .iter()
                    .map(|field| HirEnumField {
                        span: field.span,
                        ty: self.transform_ty(field.ty.clone()).kind,
                    })
                    .collect(),
            };
            variants.push(variant.clone());

            let item = HirItem {
                id: variant.id,
                span: variant.span,
                ident: Ident::dummy(),
                kind: HirItemKind::EnumVariant(variant),
                is_public: true,
                mod_id: self.current_mod_id,
                attrs: vec![],
            };

            self.map.insert_hir_item(item.id, StoredHirItem::Item(item.clone()));
            self.compiled.insert_item(item.clone());
        }

        HirEnum {
            ident: enm.ident,
            variants,
            generics: self.transform_generics(enm.generics),
            span: enm.span,
            items,
        }
    }

    pub fn transform_generics(&mut self, generics: Generics) -> HirGenerics {
        HirGenerics {
            params: generics
                .params
                .iter()
                .map(|param| HirGenericParam {
                    id: param.id,
                    name: param.ident,
                    kind: self.hir_generic_kind(param.kind.clone()),
                })
                .collect(),
            span: generics.span,
        }
    }

    pub fn transform_block(&mut self, block: Block) -> HirBlock {
        let stmts = block.stmts.iter().map(|stmt| self.transform_stmt(stmt.clone())).collect();

        HirBlock { id: block.id, span: block.span, stmts }
    }

    pub fn transform_stmt(&mut self, stmt: Stmt) -> HirStmt {
        HirStmt {
            id: stmt.id,
            span: stmt.span,
            kind: match stmt.kind {
                StmtKind::Expr(expr) => HirStmtKind::Expr(self.transform_expr(expr).0),
                StmtKind::Let(le) => HirStmtKind::Let {
                    ty: le.ty.map(|ty| self.transform_ty(ty).kind),
                    ident: le.ident,
                    value: le.value.map(|expr| self.transform_expr(expr).0),
                },
                StmtKind::If(if_expr) => HirStmtKind::If(self.transform_if_stmt(if_expr)),
                StmtKind::Match(mt) => HirStmtKind::Match(self.transform_match(mt)),
            },
        }
    }

    pub fn hir_generic_kind(&mut self, kind: GenericKind) -> HirGenericKind {
        match kind {
            GenericKind::Type { default } => {
                HirGenericKind::Type { default: default.map(|ty| self.transform_ty(ty)) }
            }
            GenericKind::NonType { ty, default } => HirGenericKind::Const {
                ty: self.transform_ty(ty),
                default: default.map(|expr| *self.transform_comptime_expr(expr).as_lit()),
            },
        }
    }

    pub fn transform_expr(&mut self, expr: Expr) -> (HirExpr, ItemId) {
        let mut fn_name: Option<String> = None;
        let mut builtin_params: Vec<HirExpr> = Vec::new();
        let mut overwrite_id: Option<ItemId> = None;

        let mut ty = HirTyKind::Placeholder;
        let mut expr = HirExpr {
            id: expr.id,
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
                ExprKind::Field(idents) => HirExprKind::Field(idents),
                ExprKind::Index(expr, index) => HirExprKind::Index(
                    Box::new(self.transform_expr(*expr).0),
                    Box::new(self.transform_expr(*index).0),
                ),
                ExprKind::Literal(lit, _) => {
                    // When literal is an integer with f32 suffix, we need to convert it to float,
                    // because C++ compiler will error
                    if lit.kind == LitKind::Integer
                        && lit.suffix.is_some_and(|s| s.to_string() == "f32")
                    {
                        let sym = lit.symbol.to_string();

                        HirExprKind::Literal(Lit {
                            kind: LitKind::Float,
                            suffix: lit.suffix,
                            symbol: Symbol::new(&format!("{sym}.0")),
                        })
                    } else {
                        HirExprKind::Literal(lit)
                    }
                }
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
                            id: ItemId::new(),
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
                ExprKind::Block(block) => HirExprKind::Block(self.transform_block(block)),
                ExprKind::Call(expr, args) => HirExprKind::Call(
                    Box::new(self.transform_expr(*expr).0),
                    args.iter().map(|arg| self.transform_expr(arg.clone()).0).collect(),
                    vec![],
                ),
                ExprKind::Comptime(expr) => HirExprKind::Comptime(ComptimeValue::Expr(Box::new(
                    self.transform_expr(*expr).0,
                ))),
                ExprKind::Array(items) => HirExprKind::Array(
                    items.iter().map(|item| self.transform_expr(item.clone()).0).collect(),
                ),
                ExprKind::Builtin(name, args) => {
                    let func = get_builtin_function(&name.to_string());
                    let new_args: &mut Vec<HirExpr> = &mut Vec::new();

                    for arg in args {
                        new_args.push(self.transform_expr(arg.clone()).0);
                    }

                    if let Some(func) = func {
                        let x = (func.func)(self.current_mod_id.as_usize(), new_args);

                        fn_name = Some(name.to_string());

                        if let Ok(val) = x {
                            ty = val.ty;
                            builtin_params = new_args.clone();
                            if val.id == expr.id {
                                overwrite_id = Some(val.id);
                            }
                            val.kind
                        } else {
                            self.ctx.emit(x.unwrap_err());

                            HirExprKind::dummy()
                        }
                    } else {
                        todo!()
                    }
                }
                ExprKind::StructConstructor(ident, generics, fields) => {
                    HirExprKind::StructConstructor(HirStructConstructor {
                        id: expr.id,
                        name: ident,
                        generics: self.transform_generic_arguments(generics),
                        fields: fields
                            .iter()
                            .map(|(ident, expr)| (*ident, self.transform_expr(expr.clone()).0))
                            .collect(),
                        field_types: HashMap::new(),
                    })
                }
                ExprKind::Match(mt) => HirExprKind::Match(self.transform_match(mt)),
                ExprKind::Path(_) => {
                    let id = self.compiled.get_assigned_path(expr.id);

                    HirExprKind::Path(id)
                }
                ExprKind::Type(ty) => HirExprKind::Type(self.transform_ty(*ty).kind),
                ExprKind::StaticAccess(_, expr) => {
                    let id = self.compiled.get_assigned_path(expr.id);

                    HirExprKind::StaticAccess(id, Box::new(self.transform_expr(*expr).0))
                }
                ExprKind::MethodCall(ident, expr) => {
                    HirExprKind::MethodCall(ident, Box::new(self.transform_expr(*expr).0))
                }
                ExprKind::Unwrap(expr) => {
                    HirExprKind::Unwrap(Box::new(self.transform_expr(*expr).0))
                }
                ExprKind::Ternary(cond, then_expr, else_expr) => HirExprKind::Ternary(
                    Box::new(self.transform_expr(*cond).0),
                    Box::new(self.transform_expr(*then_expr).0),
                    Box::new(self.transform_expr(*else_expr).0),
                ),
            },
            ty,
        };
        let id = if let Some(id) = overwrite_id { id } else { expr.id };
        expr.id = id;

        self.map.insert_hir_expr(id, expr.clone());
        if let Some(fn_name) = fn_name {
            self.compiled.expanded_by_builtins.insert(id, fn_name);
            self.compiled.builtin_args.insert(id, builtin_params);
        }

        (expr, id)
    }

    pub fn transform_match(&mut self, mt: Match) -> HirMatch {
        let arms = mt
            .arms
            .iter()
            .map(|arm| match arm {
                MatchArm::Else(block) => HirMatchArm::Else(self.transform_expr(block.clone()).0),
                MatchArm::Case(pat, block) => HirMatchArm::Case(
                    self.transform_expr(pat.clone()).0,
                    self.transform_expr(block.clone()).0,
                ),
            })
            .collect();

        HirMatch { span: mt.span, expr: Box::new(self.transform_expr(*mt.expr).0), arms }
    }

    pub fn transform_if_stmt(&mut self, if_expr: IfStmt) -> HirIfStmt {
        HirIfStmt {
            span: if_expr.span,
            condition: Box::new(self.transform_expr(*if_expr.condition).0),
            then_block: Box::new(self.transform_expr(*if_expr.then_block).0),
            else_block: if_expr
                .else_block
                .map(|else_block| Box::new(self.transform_expr(*else_block).0)),
            else_ifs: if_expr
                .else_ifs
                .iter()
                .map(|branch| HirConditionBranch {
                    condition: Box::new(self.transform_expr(*branch.condition.clone()).0),
                    block: Box::new(self.transform_expr(*branch.block.clone()).0),
                })
                .collect(),
        }
    }

    pub fn transform_generic_arguments(&mut self, generics: GenericArgs) -> HirGenericArgs {
        HirGenericArgs {
            params: generics
                .params
                .iter()
                .map(|param| HirGenericArg {
                    id: param.id,
                    ty: self.transform_ty(param.ty.clone()).kind,
                })
                .collect(),
            span: generics.span,
        }
    }

    pub fn transform_ty(&mut self, ty: Ty) -> HirTy {
        HirTy {
            id: ty.id,
            span: ty.span,
            kind: match ty.kind {
                TyKind::Ptr(ty, cnst) => {
                    HirTyKind::Ptr(Box::new(self.transform_ty(*ty).kind), cnst)
                }
                TyKind::Ref(ty, cnst) => {
                    HirTyKind::Ref(Box::new(self.transform_ty(*ty).kind), cnst)
                }
                TyKind::Primitive(primitive) => HirTyKind::Primitive(primitive),
                TyKind::Vec(ty) => HirTyKind::Vec(Box::new(self.transform_ty(*ty).kind)),
                TyKind::Mut(ty) => HirTyKind::Mut(Box::new(self.transform_ty(*ty).kind)),
                TyKind::Const(ty) => HirTyKind::Const(Box::new(self.transform_ty(*ty).kind)),
                TyKind::Ident { ident, generics } => HirTyKind::Ident {
                    ident,
                    generics: self.transform_generic_arguments(generics),
                    is_generic: false,
                },
                TyKind::Result(ty, err_ty) => HirTyKind::Result(
                    Box::new(self.transform_ty(*ty).kind),
                    Box::new(self.transform_ty(*err_ty).kind),
                ),
                TyKind::Option(ty) => HirTyKind::Option(Box::new(self.transform_ty(*ty).kind)),
                TyKind::Err(_) => panic!("on ty: {ty:?}"),
            },
        }
    }

    pub fn ret_with_error(&mut self, err: impl ToDiagnostic + 'static) -> HirTyKind {
        self.ctx.emit(Box::new(err));
        HirTyKind::err()
    }
}

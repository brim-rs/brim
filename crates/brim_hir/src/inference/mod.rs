mod errors;
pub mod scope;

use crate::{
    CompiledModules,
    expr::{HirExpr, HirExprKind, HirIfExpr},
    inference::{
        errors::{
            CannotApplyBinary, CannotApplyUnary, CannotCompare, CannotReferenceToRef,
            InvalidFunctionArgCount, NoField, OrelseExpectedOption, UnwrapNonOptional,
        },
        scope::{TypeInfo, TypeScopeManager},
    },
    items::{
        HirCallParam, HirFn, HirGenericArg, HirGenericArgs, HirGenericKind, HirGenericParam,
        HirItem, HirItemKind,
    },
    stmts::{HirStmt, HirStmtKind},
    transformer::{HirModule, HirModuleMap, StoredHirItem},
    ty::HirTyKind,
};
use brim_ast::{
    ItemId,
    expr::{BinOpKind, UnaryOp},
    item::{FunctionContext, Ident},
    token::{Lit, LitKind},
    ty::{Mutable, PrimitiveType},
};
use brim_diagnostics::diagnostic::ToDiagnostic;
use brim_middle::{ModuleId, temp_diag::TemporaryDiagnosticContext};
use brim_span::span::Span;
use errors::NoMethod;
use indexmap::IndexMap;

#[derive(Debug)]
pub struct TypeInference<'a> {
    pub hir: &'a mut HirModuleMap,
    pub ctx: InferCtx,
    pub scope_manager: TypeScopeManager,
    pub current_mod: ModuleId,
    pub temp: TemporaryDiagnosticContext,
    pub compiled: &'a mut CompiledModules,
}

#[derive(Debug, Clone)]
pub struct InferCtx {
    /// Generics available in the current scope.
    pub generics: Vec<HirGenericParam>,
}

impl InferCtx {
    pub fn new() -> Self {
        Self {
            generics: Vec::new(),
        }
    }

    pub fn push_generic(&mut self, generic: HirGenericParam) {
        self.generics.push(generic);
    }

    pub fn clear_generics(&mut self) {
        self.generics.clear();
    }
}

pub fn infer_types<'a>(
    hir: &'a mut HirModuleMap,
    compiled: &'a mut CompiledModules,
) -> TypeInference<'a> {
    let mut ti = TypeInference {
        hir,
        ctx: InferCtx::new(),
        scope_manager: TypeScopeManager::new(),
        current_mod: ModuleId::from_usize(0),
        temp: TemporaryDiagnosticContext::new(),
        compiled,
    };
    ti.infer();

    ti
}

impl<'a> TypeInference<'a> {
    pub fn infer(&mut self) {
        let inferred_modules: Vec<_> = self
            .hir
            .modules
            .clone()
            .into_iter()
            .map(|mut module| {
                self.current_mod = module.mod_id;
                self.infer_module(&mut module);
                module
            })
            .collect();

        self.hir.modules = inferred_modules;
    }

    pub fn infer_module(&mut self, module: &mut HirModule) {
        for item in &mut module.items {
            self.infer_item(item.clone());
        }
    }

    fn resolve_type_alias(&self, ty: &HirTyKind) -> HirTyKind {
        match ty {
            HirTyKind::Ref(inner, m) => {
                HirTyKind::Ref(Box::new(self.resolve_type_alias(inner)), m.clone())
            }
            HirTyKind::Ptr(inner, m) => {
                HirTyKind::Ptr(Box::new(self.resolve_type_alias(inner)), m.clone())
            }
            HirTyKind::Mut(inner) => HirTyKind::Mut(Box::new(self.resolve_type_alias(inner))),
            HirTyKind::Vec(inner) => HirTyKind::Vec(Box::new(self.resolve_type_alias(inner))),
            HirTyKind::Ident {
                ident,
                generics,
                is_generic,
            } => {
                if let Some(sym) = self
                    .compiled
                    .resolve_symbol(&ident.to_string(), self.current_mod.as_usize())
                {
                    if let HirItemKind::TypeAlias(ty_alias) = &sym.kind {
                        let resolved_ty = ty_alias.ty.resolved().as_ty();

                        if resolved_ty.can_be_directly_used() {
                            return resolved_ty.clone();
                        }
                    }
                }

                ty.clone()
            }
            HirTyKind::Result(ok, err) => {
                let resolved_ok = Box::new(self.resolve_type_alias(ok));
                let resolved_err = Box::new(self.resolve_type_alias(err));

                HirTyKind::Result(resolved_ok, resolved_err)
            }
            HirTyKind::Option(ty) => {
                let resolved_ty = Box::new(self.resolve_type_alias(ty));

                HirTyKind::Option(resolved_ty)
            }
            HirTyKind::Some(inner) => {
                let resolved_inner = Box::new(self.resolve_type_alias(inner));

                HirTyKind::Some(resolved_inner)
            }
            HirTyKind::Primitive(_)
            | HirTyKind::Placeholder
            | HirTyKind::None
            | HirTyKind::Err(_) => ty.clone(),
            _ => todo!("missing implementation for {:?}", ty),
        }
    }

    fn infer_item_inner(&mut self, id: ItemId) -> HirItem {
        let mut item = self.compiled.items.get(&id).unwrap().clone();
        match item.kind {
            HirItemKind::Fn(ref mut f) => {
                self.scope_manager.push_scope();

                for generic in &f.sig.generics.params {
                    self.ctx.push_generic(generic.clone());
                }

                for param in &mut f.sig.params.params {
                    param.ty.kind = self.update_generic(param.ty.kind.clone());

                    let param_type = param.ty.kind.clone();
                    let type_info = TypeInfo {
                        ty: param_type,
                        span: param.span.clone(),
                    };
                    self.scope_manager.declare_variable(
                        param.name.clone().to_string(),
                        type_info,
                        true,
                    );
                }

                f.sig.return_type = self.update_generic(f.sig.return_type.clone());

                if let Some(body_id) = f.body {
                    self.infer_body(body_id);
                }

                self.scope_manager.pop_scope();
            }
            HirItemKind::Struct(ref mut str) => {
                self.scope_manager.push_scope();

                for generic in &str.generics.params {
                    self.ctx.push_generic(generic.clone());
                }

                for field in str.fields.iter_mut() {
                    field.ty = self.update_generic(field.ty.clone());
                }

                for (_, id) in str.items.iter_mut() {
                    self.infer_item(id.clone());
                }

                self.scope_manager.pop_scope();
            }
            HirItemKind::Enum(ref mut en) => {
                self.scope_manager.push_scope();

                for generic in &en.generics.params {
                    self.ctx.push_generic(generic.clone());
                }

                for variant in en.variants.iter_mut() {
                    for field in variant.fields.iter_mut() {
                        field.ty = self.update_generic(field.ty.clone());
                    }
                }

                for (_, id) in en.items.iter_mut() {
                    self.infer_item(id.clone());
                }

                self.scope_manager.pop_scope();
            }
            HirItemKind::External(ref ext) => {
                for item in ext.items.clone() {
                    self.infer_item(item.clone());
                }
            }
            HirItemKind::Namespace(_) | HirItemKind::Use(_) | HirItemKind::TypeAlias(_) => {}
            _ => todo!("missing implementation for {:?}", item.kind),
        }

        item.clone()
    }

    fn update_generic(&mut self, mut ty: HirTyKind) -> HirTyKind {
        if let Some(_) = self.is_generic(&ty) {
            let (ident, mut generics) = ty.clone().as_ident().unwrap();

            for generic in generics.params.iter_mut() {
                generic.ty = self.update_generic(generic.ty.clone());
            }

            ty = HirTyKind::Ident {
                ident,
                generics,
                is_generic: true,
            };
        } else if let Some((ident, mut generics)) = ty.clone().as_ident() {
            for generic in generics.params.iter_mut() {
                generic.ty = self.update_generic(generic.ty.clone());
            }

            ty = self.resolve_type_alias(&HirTyKind::Ident {
                ident,
                generics,
                is_generic: false,
            })
        } else {
            ty = self.resolve_type_alias(&ty)
        }

        ty
    }

    fn infer_item(&mut self, item: ItemId) {
        let item = self.infer_item_inner(item);

        self.hir
            .hir_items
            .insert(item.id, StoredHirItem::Item(item.clone()));
        self.compiled.items.insert(item.id, item);
    }

    fn infer_body(&mut self, body_id: ItemId) {
        let mut expr = self.hir.get_expr_mut(body_id).clone();

        self.infer_expr(&mut expr);
        *self.hir.get_expr_mut(body_id) = expr;
    }

    fn infer_stmt(&mut self, stmt: &mut HirStmt) {
        match &mut stmt.kind {
            HirStmtKind::Expr(expr) => self.infer_expr(expr),
            HirStmtKind::Let {
                ty: let_ty,
                value,
                ident,
            } => {
                if let Some(value) = value {
                    self.infer_expr(value);

                    if let None = let_ty {
                        let_ty.replace(value.ty.clone());
                    }
                }
                let mut ty = let_ty.clone().unwrap();

                ty = self.update_generic(ty);
                if let Some(val) = value {
                    self.try_promote_type(&mut val.ty, &ty, true);
                }
                let_ty.replace(ty.clone());

                self.scope_manager.declare_variable(
                    ident.to_string(),
                    TypeInfo {
                        ty,
                        span: stmt.span.clone(),
                    },
                    true,
                );
            }
            HirStmtKind::If(if_expr) => {
                self.infer_expr(if_expr);
            }
        }

        self.hir
            .hir_items
            .insert(stmt.id, StoredHirItem::Stmt(stmt.clone()));
    }

    fn is_generic(&self, ty: &HirTyKind) -> Option<HirGenericParam> {
        if let HirTyKind::Ident { ident, .. } = ty {
            self.ctx
                .generics
                .iter()
                .find(|g| g.name.to_string() == *ident.to_string())
                .cloned()
        } else {
            None
        }
    }

    fn assign_generic(&self, expr: &mut HirExpr) {
        if let Some(generic) = self.is_generic(&expr.ty) {
            if let HirGenericKind::Type {
                default: Some(default),
            } = generic.kind
            {
                expr.ty = default.kind;
            } else {
                expr.ty = HirTyKind::Ident {
                    ident: generic.name.clone(),
                    generics: HirGenericArgs::empty(),
                    is_generic: true,
                }
            }
        }
    }

    fn infer_expr(&mut self, expr: &mut HirExpr) {
        let kind = match &mut expr.kind {
            HirExprKind::Unary(op, operand) => {
                self.infer_expr(operand);

                match (op, &operand.ty) {
                    (UnaryOp::Try, ty) => ty,
                    (UnaryOp::Minus, ty) => {
                        if ty.is_numeric() {
                            ty
                        } else {
                            &self.ret_with_error(CannotApplyUnary {
                                span: (expr.span.clone(), self.current_mod.as_usize()),
                                op: UnaryOp::Minus,
                                ty: ty.clone(),
                            })
                        }
                    }
                    (UnaryOp::Deref, ty) => {
                        if ty.can_be_dereferenced() {
                            ty
                        } else {
                            &self.ret_with_error(CannotApplyUnary {
                                span: (expr.span.clone(), self.current_mod.as_usize()),
                                op: UnaryOp::Deref,
                                ty: ty.clone(),
                            })
                        }
                    }
                    (UnaryOp::Not, _) => &HirTyKind::Primitive(PrimitiveType::Bool),
                    (UnaryOp::Ref, ty) => {
                        if ty.is_mutable() {
                            &HirTyKind::Ref(Box::new(ty.clone()), Mutable::Yes)
                        } else {
                            &HirTyKind::Ref(Box::new(ty.clone()), Mutable::No)
                        }
                    }
                }
            }
            HirExprKind::Block(block) => &{
                for stmt in &mut block.stmts {
                    self.infer_stmt(stmt);
                }

                block
                    .stmts
                    .last()
                    .and_then(|stmt| stmt.can_be_used_for_inference())
                    .unwrap_or(HirTyKind::void())
            },
            HirExprKind::Return(expr) => &{
                self.infer_expr(expr);
                expr.ty.clone()
            },
            HirExprKind::Binary(lhs, op, rhs) => {
                self.infer_expr(lhs);
                self.infer_expr(rhs);

                self.assign_generic(lhs);
                self.assign_generic(rhs);

                let opc = op.clone();
                match (op, &lhs.ty, &rhs.ty) {
                    // Numeric operations
                    (
                        BinOpKind::Plus
                        | BinOpKind::Minus
                        | BinOpKind::Multiply
                        | BinOpKind::Divide
                        | BinOpKind::Modulo
                        | BinOpKind::Power
                        | BinOpKind::Caret
                        | BinOpKind::ShiftRight
                        | BinOpKind::ShiftLeft
                        | BinOpKind::And
                        | BinOpKind::Or,
                        l,
                        r,
                    ) => {
                        if l.is_numeric() && r.is_numeric() {
                            let ty =
                                PrimitiveType::promote_type(&l.to_primitive(), &r.to_primitive())
                                    .expect(&format!("failed for types: {:?} and {:?}", l, r));

                            &HirTyKind::Primitive(ty)
                        } else {
                            &self.ret_with_error(CannotApplyBinary {
                                span: (expr.span.clone(), self.current_mod.as_usize()),
                                op: opc,
                                lhs: l.clone(),
                                rhs: r.clone(),
                            })
                        }
                    }

                    (
                        BinOpKind::Lt
                        | BinOpKind::Le
                        | BinOpKind::EqEq
                        | BinOpKind::Ne
                        | BinOpKind::Ge
                        | BinOpKind::Gt,
                        l,
                        r,
                    ) => {
                        if l.can_be_logically_compared_to(r) {
                            &HirTyKind::Primitive(PrimitiveType::Bool)
                        } else {
                            &self.ret_with_error(CannotCompare {
                                span: (expr.span.clone(), self.current_mod.as_usize()),
                                op: opc,
                                lhs: l.clone(),
                                rhs: r.clone(),
                            })
                        }
                    }

                    (BinOpKind::AndAnd | BinOpKind::OrOr, l, r) => {
                        if l.is_bool() && r.is_bool() {
                            &HirTyKind::Primitive(PrimitiveType::Bool)
                        } else {
                            &self.ret_with_error(CannotApplyUnary {
                                span: (expr.span.clone(), self.current_mod.as_usize()),
                                op: UnaryOp::Not,
                                ty: l.clone(),
                            })
                        }
                    }

                    (BinOpKind::OrElse, l, r) => &match l.is_option() {
                        Some(x) => x,
                        None => self.ret_with_error(OrelseExpectedOption {
                            span: (expr.span.clone(), self.current_mod.as_usize()),
                            ty: l.clone(),
                        }),
                    },
                }
            }
            HirExprKind::Var(name) => {
                let var = self.scope_manager.resolve_variable(&name.to_string());

                if let Some(var) = var {
                    &var.ty
                } else {
                    panic!()
                }
            }
            HirExprKind::Call(ident, args, params) => {
                let func_ident = ident.as_ident().unwrap().to_string();
                let func = match self
                    .compiled
                    .resolve_symbol(&func_ident, self.current_mod.as_usize())
                    .cloned()
                {
                    Some(f) => f,
                    None => panic!("'{}' function not found", func_ident),
                };

                let fn_def = match &func.kind {
                    HirItemKind::Fn(f) => f,
                    _ => panic!("not a func"),
                };

                &self.infer_call_expr(func.ident, fn_def, args, params)
            }
            HirExprKind::StructConstructor(hir_struct) => {
                let fields = &mut hir_struct.fields;
                let ident = &hir_struct.name;

                let str = self
                    .compiled
                    .resolve_symbol(&ident.to_string(), self.current_mod.as_usize())
                    .unwrap()
                    .clone();

                let str = match &str.kind {
                    HirItemKind::Struct(s) => s,
                    _ => panic!("not a struct"),
                };

                let mut generic_types: IndexMap<String, HirTyKind> = IndexMap::new();

                for (ident, expr) in fields.iter_mut() {
                    let str_field = str.get_field(&ident.to_string()).unwrap();
                    self.infer_expr(expr);

                    if let Some(generic_param) = str.generics.is_generic(&str_field.ty) {
                        match &generic_param.kind {
                            HirGenericKind::Type { default } => {
                                let inferred_type = if let Some(def) = default {
                                    def.kind.clone()
                                } else if expr.ty == expr.ty {
                                    expr.ty.clone()
                                } else {
                                    expr.ty.clone()
                                };

                                // If another field has a generic that was already inferred, we don't want to overwrite it.
                                if let Some(existing) = generic_types
                                    .insert(generic_param.name.to_string(), inferred_type.clone())
                                {
                                    hir_struct.field_types.insert(ident.clone(), existing);
                                } else {
                                    hir_struct
                                        .field_types
                                        .insert(ident.clone(), inferred_type.clone());
                                }
                            }
                            HirGenericKind::Const { .. } => {
                                hir_struct
                                    .field_types
                                    .insert(ident.clone(), expr.ty.clone());
                            }
                        }
                    } else {
                        let field_ty = str_field.ty.clone();

                        self.try_promote_type(&mut expr.ty, &field_ty, false);

                        hir_struct
                            .field_types
                            .insert(ident.clone(), expr.ty.clone());

                        hir_struct
                            .field_types
                            .insert(ident.clone(), expr.ty.clone());
                    }
                }

                if let Some(generic_param) = str.generics.is_generic(&expr.ty) {
                    match &generic_param.kind {
                        HirGenericKind::Type { default } => {
                            let inferred_type = if let Some(def) = default {
                                def.kind.clone()
                            } else if expr.ty == expr.ty {
                                expr.ty.clone()
                            } else {
                                expr.ty.clone()
                            };

                            generic_types
                                .insert(generic_param.name.to_string(), inferred_type.clone());
                        }
                        HirGenericKind::Const { .. } => {}
                    }
                }

                self.replace_generics_recursive(&mut expr.ty, &generic_types);

                // Generics have to be sorted in the way they are defined in the struct.
                let mut sorted_generics = IndexMap::new();

                for generic in &str.generics.params {
                    if let Some(ty) = generic_types.get(&generic.name.to_string()) {
                        sorted_generics.insert(generic.id, ty.clone());
                    }
                }
                let collected: Vec<HirGenericArg> = sorted_generics
                    .iter()
                    .map(|(id, ty)| HirGenericArg {
                        id: *id,
                        ty: ty.clone(),
                    })
                    .collect();

                &HirTyKind::Ident {
                    ident: ident.clone(),
                    generics: HirGenericArgs::new(expr.span.clone(), collected),
                    is_generic: false,
                }
            }
            HirExprKind::Literal(lit) => match lit.kind {
                LitKind::Str => &HirTyKind::Primitive(PrimitiveType::String),
                LitKind::Byte => &HirTyKind::Primitive(PrimitiveType::U8),
                LitKind::ByteStr => {
                    &HirTyKind::Vec(Box::new(HirTyKind::Primitive(PrimitiveType::U8)))
                }
                LitKind::Char => &HirTyKind::Primitive(PrimitiveType::Char),
                LitKind::Bool => &HirTyKind::Primitive(PrimitiveType::Bool),
                LitKind::Integer => &self.ty_with_suffix(lit, PrimitiveType::I32),
                LitKind::Float => &self.ty_with_suffix(lit, PrimitiveType::F64),
                LitKind::CStr => todo!("CStr"),
                _ => unreachable!("literal error: {:#?}", lit),
            },
            HirExprKind::If(if_expr) => &self.infer_if_expr(if_expr),
            HirExprKind::Builtin(_, args) => {
                for arg in args {
                    self.infer_expr(arg);
                }

                &HirTyKind::Placeholder
            }
            HirExprKind::Type(ty) => &self.resolve_type_alias(&ty),
            HirExprKind::Assign(lhs, rhs) => {
                self.infer_expr(lhs);
                self.infer_expr(rhs);

                &HirTyKind::void()
            }
            HirExprKind::StaticAccess(id, expr) => {
                let citem = self.compiled.get_item(id.clone());

                let generics = if let Some(str) = citem.as_struct() {
                    str.generics.clone().params
                } else if let Some(str) = citem.as_enum() {
                    str.generics.clone().params
                } else {
                    Vec::new()
                };

                match &mut expr.kind {
                    HirExprKind::Call(ident, args, params) => {
                        let ident = ident.as_ident().unwrap().clone();
                        if let Some(str) = citem.as_struct() {
                            let method = str.get_item(ident);
                            let func = self.compiled.get_item(method.unwrap()).as_fn().clone();

                            for generic in generics {
                                self.ctx.push_generic(generic);
                            }

                            &self.infer_call_expr(ident, &func, args, params)
                        } else if let Some(str) = citem.as_enum() {
                            if let Some(_) = str.get_variant(ident) {
                                &HirTyKind::Ident {
                                    ident: citem.ident.clone(),
                                    generics: HirGenericArgs::empty(),
                                    is_generic: false,
                                }
                            } else if let Some(item) = str.get_item(ident) {
                                let func = self.compiled.get_item(*item).as_fn().clone();

                                for generic in generics {
                                    self.ctx.push_generic(generic);
                                }

                                &self.infer_call_expr(ident, &func, args, params)
                            } else {
                                todo!()
                            }
                        } else {
                            todo!()
                        }
                    }
                    _ => {
                        self.infer_expr(expr);

                        &expr.ty
                    }
                }
            }
            HirExprKind::MethodCall(idents, expr) => {
                let idents_clone = idents.clone();
                let item = self.resolve_method_from_idents(idents, expr, None);
                *idents = idents_clone; // Restore the original idents after resolution

                if let Some(item) = item {
                    let method_fn = item.as_fn().clone();
                    let return_type = method_fn.sig.return_type.clone();

                    &match &mut expr.kind {
                        HirExprKind::Call(call_ident, args, params) => {
                            let ident_clone = call_ident.as_ident().unwrap().clone();

                            let mut method_fn_for_inference = method_fn.clone();
                            if !method_fn_for_inference.is_static()
                                && method_fn_for_inference.ctx == FunctionContext::Method
                            {
                                method_fn_for_inference.sig.params.params.remove(0);
                            }

                            let inferred_type = self.infer_call_expr(
                                ident_clone,
                                &method_fn_for_inference,
                                args,
                                params,
                            );
                            expr.ty = inferred_type.clone();

                            self.hir
                                .hir_items
                                .insert(expr.id, StoredHirItem::Expr(*expr.clone()));

                            self.hir.update_builtins(*expr.clone());

                            return_type
                        }
                        _ => unreachable!(),
                    }
                } else {
                    &HirTyKind::err()
                }
            }
            HirExprKind::Unwrap(expr) => {
                self.infer_expr(expr);

                &match &expr.ty {
                    HirTyKind::Option(ty) => ty.clone(),
                    HirTyKind::Some(ty) => ty.clone(),
                    _ => Box::from(self.ret_with_error(UnwrapNonOptional {
                        span: (expr.span.clone(), self.current_mod.as_usize()),
                        ty: expr.ty.clone(),
                    })),
                }
            }
            HirExprKind::Field(idents) => {
                let idents_clone = idents.clone();
                let field_ty = self.resolve_field_from_idents(idents);
                *idents = idents_clone;

                if let Some(field_type) = field_ty {
                    expr.ty = field_type;

                    self.hir
                        .hir_items
                        .insert(expr.id, StoredHirItem::Expr(expr.clone()));

                    self.hir.update_builtins(expr.clone());

                    &expr.ty
                } else {
                    expr.ty = HirTyKind::err();
                    &expr.ty
                }
            }
            HirExprKind::Dummy => &expr.ty.clone(),
            _ => todo!("infer_expr: {:?}", expr.kind),
        };

        if expr.ty.is_placeholder() {
            match expr.ty {
                HirTyKind::ResultOk(_) => expr.ty = HirTyKind::ResultOk(Box::new(kind.clone())),
                HirTyKind::ResultErr(_) => expr.ty = HirTyKind::ResultErr(Box::new(kind.clone())),
                HirTyKind::Some(_) => expr.ty = HirTyKind::Some(Box::new(kind.clone())),
                _ => expr.ty = kind.clone(),
            }
        }

        expr.ty = self.resolve_type_alias(&expr.ty);

        self.hir
            .hir_items
            .insert(expr.id, StoredHirItem::Expr(expr.clone()));

        self.hir.update_builtins(expr.clone());
    }

    pub fn resolve_field_from_idents(&mut self, idents: &mut Vec<Ident>) -> Option<HirTyKind> {
        if idents.is_empty() {
            return None;
        }

        let first = idents.first().unwrap().to_string();
        let var = self.scope_manager.resolve_variable(&first).cloned();

        if let Some(var) = var {
            idents.remove(0);

            if idents.is_empty() {
                return Some(var.ty);
            }

            return self.resolve_field_access(var.ty, idents);
        } else {
            let error_span = idents.first().unwrap().span;
            self.temp.emit_impl(NoField {
                span: (error_span, self.current_mod.as_usize()),
                field: first,
                ty: HirTyKind::err(),
            });

            None
        }
    }

    fn resolve_field_access(
        &mut self,
        ty: HirTyKind,
        idents: &mut Vec<Ident>,
    ) -> Option<HirTyKind> {
        if idents.is_empty() {
            return Some(ty);
        }
        let current_ident = idents.first().unwrap().clone();
        let error_span = current_ident.span;

        if let Some(type_ident) = ty.is_ident() {
            let type_name = type_ident.to_string();

            let sym = self
                .compiled
                .resolve_symbol(&type_name, self.current_mod.as_usize());

            if let Some(sym) = sym {
                if let Some(struct_data) = sym.as_struct() {
                    for field in &struct_data.fields {
                        if field.ident == current_ident {
                            idents.remove(0);

                            if idents.is_empty() {
                                return Some(field.ty.clone());
                            }

                            return self.resolve_field_access(field.ty.clone(), idents);
                        }
                    }
                }
            }
        } else if ty.is_vector() {
            if current_ident.to_string() == "len" {
                return Some(HirTyKind::Primitive(PrimitiveType::Usize));
            }
        }

        self.temp.emit_impl(NoField {
            span: (error_span, self.current_mod.as_usize()),
            field: current_ident.to_string(),
            ty: ty.clone(),
        });

        None
    }

    pub fn infer_if_expr(&mut self, if_expr: &mut HirIfExpr) -> HirTyKind {
        self.infer_expr(&mut if_expr.condition);
        self.infer_expr(&mut if_expr.then_block);

        if let Some(else_block) = &mut if_expr.else_block {
            self.infer_expr(else_block);
        }

        for branch in &mut if_expr.else_ifs {
            self.infer_expr(&mut branch.condition);
            self.infer_expr(&mut branch.block);
        }

        if_expr.then_block.ty.clone()
    }

    pub fn resolve_method_from_idents(
        &mut self,
        idents: &mut Vec<Ident>,
        expr: &mut Box<HirExpr>,
        last: Option<TypeInfo>,
    ) -> Option<HirItem> {
        if idents.is_empty() {
            return None;
        }

        let first = idents.first().unwrap().to_string();

        if let Some(type_info) = last {
            return self.resolve_from_type(type_info, idents, expr);
        }

        let var = self.scope_manager.resolve_variable(&first).cloned();
        let function_call = expr.as_call().as_ident().unwrap();

        if let Some(var) = var {
            idents.remove(0);

            if idents.is_empty() {
                return None;
            }

            return self.resolve_member_access(var.ty, idents, expr, function_call.span);
        } else {
            self.temp.emit_impl(NoMethod {
                span: (function_call.span, self.current_mod.as_usize()),
                method: first,
                ty: HirTyKind::err(),
            });

            None
        }
    }

    fn resolve_from_type(
        &mut self,
        type_info: TypeInfo,
        idents: &mut Vec<Ident>,
        expr: &mut Box<HirExpr>,
    ) -> Option<HirItem> {
        if let Some(ident) = type_info.ty.is_ident() {
            let ident_str = ident.to_string();

            let sym = self
                .compiled
                .resolve_symbol(&ident_str, self.current_mod.as_usize());

            if let Some(sym) = sym {
                return self.resolve_member_access(
                    type_info.ty.clone(),
                    idents,
                    expr,
                    idents.first().unwrap().span,
                );
            }
        }

        self.temp.emit_impl(NoMethod {
            span: (idents.first().unwrap().span, self.current_mod.as_usize()),
            method: idents.first().unwrap().to_string(),
            ty: type_info.ty.clone(),
        });

        None
    }

    fn resolve_member_access(
        &mut self,
        ty: HirTyKind,
        idents: &mut Vec<Ident>,
        expr: &mut Box<HirExpr>,
        error_span: Span,
    ) -> Option<HirItem> {
        let current_ident = idents.first().unwrap().clone();

        if let Some(type_ident) = ty.is_ident() {
            let type_name = type_ident.to_string();

            let sym = self
                .compiled
                .resolve_symbol(&type_name, self.current_mod.as_usize());

            if let Some(sym) = sym {
                let method_id = sym.as_struct().unwrap().get_item(current_ident.clone());

                if let Some(method_id) = method_id {
                    idents.remove(0);

                    let item = self.compiled.get_item(method_id).clone();

                    if idents.is_empty() {
                        return Some(item);
                    } else {
                        let method_fn = item.as_fn();
                        let return_type = method_fn.sig.return_type.clone();

                        let next_type = TypeInfo {
                            ty: return_type,
                            span: idents.first().unwrap().span,
                        };

                        return self.resolve_method_from_idents(idents, expr, Some(next_type));
                    }
                }

                let struct_data = sym.as_struct().unwrap();
                for field in &struct_data.fields {
                    if field.ident == current_ident {
                        idents.remove(0);

                        if idents.is_empty() {
                            return None;
                        }

                        let next_type = TypeInfo {
                            ty: field.ty.clone(),
                            span: idents.first().unwrap().span,
                        };

                        return self.resolve_method_from_idents(idents, expr, Some(next_type));
                    }
                }
            }
        }

        self.temp.emit_impl(NoMethod {
            span: (error_span, self.current_mod.as_usize()),
            method: current_ident.to_string(),
            ty: ty.clone(),
        });

        None
    }

    pub fn infer_call_expr(
        &mut self,
        ident: Ident,
        fn_def: &HirFn,
        args: &mut Vec<HirExpr>,
        params: &mut Vec<HirCallParam>,
    ) -> HirTyKind {
        let func_params = fn_def.sig.params.params.clone();
        for generic in fn_def.sig.generics.params.clone() {
            self.ctx.push_generic(generic);
        }
        let mut generic_types: IndexMap<String, HirTyKind> = IndexMap::new();

        let mut req_params = func_params.clone().len();

        if !fn_def.is_static() {
            req_params -= 1; // Static methods don't count the self parameter
        }

        if args.len() != req_params {
            let span = if let Some(last) = args.last() {
                args[0].span.to(last.span)
            } else {
                args[0].span
            };

            self.temp.emit_impl(InvalidFunctionArgCount {
                span: (span, self.current_mod.as_usize()),
                expected: req_params as u16,
                found: args.len() as u16,
            });
        }

        for (arg, fn_param) in args.iter_mut().zip(func_params) {
            self.infer_expr(arg);
            if let Some(generic_param) = self.is_generic(&fn_param.ty.kind) {
                match &generic_param.kind {
                    HirGenericKind::Type { default } => {
                        let inferred_type = if let Some(def) = default {
                            def.kind.clone()
                        } else if arg.ty == fn_param.ty.kind {
                            arg.ty.clone()
                        } else {
                            arg.ty.clone()
                        };
                        let mut final_ty = inferred_type.clone();
                        if let Some(existing) = generic_types
                            .insert(generic_param.name.to_string(), inferred_type.clone())
                        {
                            final_ty = existing;
                        }
                        params.push(HirCallParam {
                            span: fn_param.span,
                            name: ident,
                            ty: final_ty,
                            from_generic: Some(generic_param),
                        });
                    }
                    HirGenericKind::Const { .. } => {
                        params.push(HirCallParam {
                            span: fn_param.span,
                            name: ident,
                            ty: arg.ty.clone(),
                            from_generic: Some(generic_param),
                        });
                    }
                }
            } else {
                let param_ty = fn_param.ty.kind.clone();

                self.try_promote_type(&mut arg.ty, &param_ty, false);

                params.push(HirCallParam {
                    span: fn_param.span,
                    name: ident,
                    ty: param_ty,
                    from_generic: None,
                });
            }
        }

        let mut ty = fn_def.sig.return_type.clone();
        self.replace_generics_recursive(&mut ty, &generic_types);

        self.resolve_type_alias(&ty)
    }

    pub fn try_promote_type(
        &self,
        source_ty: &mut HirTyKind,
        target_ty: &HirTyKind,
        is_let_stmt: bool,
    ) -> bool {
        if *source_ty == *target_ty {
            return true;
        }

        if let (HirTyKind::Primitive(source_prim), HirTyKind::Primitive(target_prim)) =
            (source_ty.clone(), target_ty)
        {
            if is_let_stmt && PrimitiveType::can_initialize_with_type(&source_prim, target_prim) {
                *source_ty = HirTyKind::Primitive(target_prim.clone());
                return true;
            } else if let Some(promoted) = PrimitiveType::promote_type(&source_prim, target_prim) {
                *source_ty = HirTyKind::Primitive(promoted);
                return true;
            }
        }

        false
    }

    pub fn replace_generics_recursive(
        &self,
        ty: &mut HirTyKind,
        generic_types: &IndexMap<String, HirTyKind>,
    ) {
        match ty {
            HirTyKind::Ref(inner, _) => {
                self.replace_generics_recursive(inner, generic_types);
            }
            HirTyKind::Ptr(inner, _) => {
                self.replace_generics_recursive(inner, generic_types);
            }
            HirTyKind::Mut(inner) => {
                self.replace_generics_recursive(inner, generic_types);
            }
            HirTyKind::Vec(inner) => {
                self.replace_generics_recursive(inner, generic_types);
            }
            HirTyKind::Ident {
                ident,
                generics,
                is_generic,
            } => {
                let ident_string = ident.to_string();
                if *is_generic {
                    if let Some(replacement) = generic_types.get(&ident_string) {
                        *ty = replacement.clone();
                        return;
                    }
                }

                for param in &mut generics.params {
                    self.replace_generics_recursive(&mut param.ty, generic_types);
                }
            }
            HirTyKind::Result(ok, err) => {
                self.replace_generics_recursive(ok, generic_types);
                self.replace_generics_recursive(err, generic_types);
            }
            HirTyKind::Option(inner) => {
                self.replace_generics_recursive(inner, generic_types);
            }
            HirTyKind::Some(inner) => {
                self.replace_generics_recursive(inner, generic_types);
            }

            HirTyKind::Primitive(_) | HirTyKind::Placeholder => {}

            _ => todo!("missing implementation for {:?}", ty),
        }
    }

    pub fn ty_with_suffix(&mut self, lit: &Lit, def: PrimitiveType) -> HirTyKind {
        if let Some(suf) = lit.suffix {
            let ty = PrimitiveType::try_from_string(suf.to_string()).unwrap();

            HirTyKind::Primitive(ty)
        } else {
            HirTyKind::Primitive(def)
        }
    }

    pub fn ret_with_error(&mut self, err: impl ToDiagnostic + 'static) -> HirTyKind {
        self.temp.emit(Box::new(err));
        HirTyKind::err()
    }
}

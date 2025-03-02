mod errors;
pub mod scope;

use crate::{
    CompiledModules,
    builtin::expand_builtins,
    expr::{HirExpr, HirExprKind},
    inference::{
        errors::{CannotApplyBinary, CannotApplyUnary, CannotCompare},
        scope::{TypeInfo, TypeScopeManager},
    },
    items::{
        HirCallParam, HirGenericArg, HirGenericArgs, HirGenericKind, HirGenericParam, HirItem,
        HirItemKind,
    },
    stmts::{HirStmt, HirStmtKind},
    transformer::{HirModule, HirModuleMap, StoredHirItem},
    ty::HirTyKind,
};
use brim_ast::{
    ItemId,
    expr::{BinOpKind, UnaryOp},
    token::{Lit, LitKind},
    ty::PrimitiveType,
};
use brim_diagnostics::diagnostic::ToDiagnostic;
use brim_middle::{ModuleId, temp_diag::TemporaryDiagnosticContext};
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
    expand_builtins(hir);

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
            self.infer_item(item);
        }
    }

    fn infer_item(&mut self, item: &mut HirItem) {
        match &mut item.kind {
            HirItemKind::Fn(f) => {
                self.scope_manager.push_scope();

                self.ctx.clear_generics();
                for generic in &f.sig.generics.params {
                    self.ctx.push_generic(generic.clone());
                }

                for param in &f.sig.params.params {
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

                if let Some(body_id) = f.body {
                    self.infer_body(body_id);
                }

                self.scope_manager.pop_scope(); // Reset scope after function
            }
            HirItemKind::Struct(str) => {
                for field in str.fields.iter_mut() {
                    let ty = if let Some(name) = field.ty.as_ident()
                        && let None = str.generics.is_generic(&field.ty)
                    {
                        let sym = self
                            .compiled
                            .resolve_symbol(&name.to_string(), self.current_mod.as_usize())
                            .expect(format!("{} not found", name).as_str());

                        if let HirItemKind::TypeAlias(ty) = &sym.kind {
                            if ty.ty.can_be_directly_used() {
                                ty.ty.clone()
                            } else {
                                field.ty.clone()
                            }
                        } else {
                            field.ty.clone()
                        }
                    } else {
                        field.ty.clone()
                    };

                    field.ty = ty;
                }
            }
            _ => {}
        }

        self.hir
            .hir_items
            .insert(item.id, StoredHirItem::Item(item.clone()));
    }

    fn infer_body(&mut self, body_id: ItemId) {
        let mut expr = self.hir.get_expr_mut(body_id).clone();

        self.infer_expr(&mut expr);
        *self.hir.get_expr_mut(body_id) = expr;
    }

    fn infer_stmt(&mut self, stmt: &mut HirStmt) {
        match &mut stmt.kind {
            HirStmtKind::Expr(expr) => self.infer_expr(expr),
            HirStmtKind::Let { ty, value, ident } => {
                if let Some(value) = value {
                    self.infer_expr(value);

                    if let None = ty {
                        ty.replace(value.ty.clone());
                    }
                }

                self.scope_manager.declare_variable(
                    ident.to_string(),
                    TypeInfo {
                        ty: ty.clone().unwrap(),
                        span: stmt.span.clone(),
                    },
                    true,
                );
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

                // The generics will be inferred later, so we can't infer the type yet.
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
                                    .unwrap();

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
                    None => panic!("handle this better"),
                };

                let fn_def = match &func.kind {
                    HirItemKind::Fn(f) => f,
                    _ => panic!("not a func"),
                };
                let func_params = fn_def.sig.params.params.clone();
                let mut generic_types: IndexMap<String, HirTyKind> = IndexMap::new();

                for (arg, fn_param) in args.iter_mut().zip(func_params) {
                    self.infer_expr(arg);

                    if let Some(generic_param) = fn_def.sig.generics.is_generic(&fn_param.ty.kind) {
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
                                    name: func.ident,
                                    ty: final_ty,
                                    from_generic: Some(generic_param),
                                });
                            }
                            HirGenericKind::Const { .. } => {
                                params.push(HirCallParam {
                                    span: fn_param.span,
                                    name: func.ident,
                                    ty: arg.ty.clone(),
                                    from_generic: Some(generic_param),
                                });
                            }
                        }
                    } else {
                        params.push(HirCallParam {
                            span: fn_param.span,
                            name: func.ident,
                            ty: fn_param.ty.kind.clone(),
                            from_generic: None,
                        });
                    }
                }

                let res_type = fn_def.sig.return_type.clone();
                let mut ty = if let Some(ret_ty_param) = fn_def.sig.generics.is_generic(&res_type) {
                    if let Some(inferred_type) = generic_types.get(&ret_ty_param.name.to_string()) {
                        inferred_type.clone()
                    } else {
                        match &ret_ty_param.kind {
                            HirGenericKind::Type { default: Some(def) } => def.kind.clone(),
                            _ => res_type,
                        }
                    }
                } else {
                    res_type
                };

                self.replace_generics(&mut ty, &generic_types);
                &ty.clone()
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
                        println!("{}: {:?}", ident, str_field.ty);
                        hir_struct
                            .field_types
                            .insert(ident.clone(), str_field.ty.clone());
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

                self.replace_generics(&mut expr.ty, &generic_types);

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
                LitKind::Str => &HirTyKind::Primitive(PrimitiveType::Str),
                LitKind::Byte => &HirTyKind::Primitive(PrimitiveType::U8),
                LitKind::ByteStr => &HirTyKind::Array(
                    Box::new(HirTyKind::Primitive(PrimitiveType::U8)),
                    lit.symbol.to_string().len(),
                ),
                LitKind::Char => &HirTyKind::Primitive(PrimitiveType::Char),
                LitKind::Bool => &HirTyKind::Primitive(PrimitiveType::Bool),
                LitKind::Integer => &self.ty_with_suffix(lit, PrimitiveType::I32),
                LitKind::Float => &self.ty_with_suffix(lit, PrimitiveType::F64),
                LitKind::CStr => todo!("CStr"),
                _ => unreachable!("literal error: {:#?}", lit),
            },
            HirExprKind::If(if_expr) => {
                self.infer_expr(&mut if_expr.condition);
                self.infer_expr(&mut if_expr.then_block);

                if let Some(else_block) = &mut if_expr.else_block {
                    self.infer_expr(else_block);
                }

                for branch in &mut if_expr.else_ifs {
                    self.infer_expr(&mut branch.condition);
                    self.infer_expr(&mut branch.block);
                }

                // For now, we will just use placeholder until we figure out how to compare types.
                &HirTyKind::Placeholder
            }
            HirExprKind::Builtin(_, args) => {
                for arg in args {
                    self.infer_expr(arg);
                }

                &HirTyKind::Placeholder
            }
            _ => todo!("infer_expr: {:?}", expr.kind),
        };
        expr.ty = kind.clone();

        self.hir
            .hir_items
            .insert(expr.id, StoredHirItem::Expr(expr.clone()));
    }

    /// Replace generic types with inferred types.
    pub fn replace_generics(&self, ty: &mut HirTyKind, generics: &IndexMap<String, HirTyKind>) {
        match ty {
            HirTyKind::Ident { ident, .. } => {
                if generics.contains_key(&ident.to_string()) {
                    *ty = generics.get(&ident.to_string()).unwrap().clone();
                }
            }
            HirTyKind::Array(ty, _) => self.replace_generics(ty, generics),
            HirTyKind::Vec(ty) => self.replace_generics(ty, generics),
            _ => {}
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

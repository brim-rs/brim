mod errors;
pub mod scopes;

use crate::name::{
    errors::{
        AccessOutsideComptime, InvalidPathAccess, NamespaceMissingSymbol, NoVariantOrItemInEnum,
        UndeclaredFunction, UndeclaredStruct, UndeclaredVariable,
    },
    scopes::Scope,
};
use brim_ast::{
    ItemId,
    expr::{Expr, ExprKind},
    item::{Block, FnDecl, GenericParam, Ident, Item, ItemKind, Struct, TypeAlias, TypeAliasValue},
    stmts::Let,
    ty::{Ty, TyKind},
};
use brim_diagnostics::diag_opt;
use brim_hir::CompiledModules;
use brim_middle::{
    GlobalSymbol, SimpleModules, builtins::BUILTIN_FUNCTIONS, lints::Lints, modules::ModuleMap,
    temp_diag::TemporaryDiagnosticContext, walker::AstWalker,
};
use brim_span::span::Span;
use convert_case::{Case, Casing};
use errors::{
    IdentifierNotFound, InvalidReceiverForStaticAccess, ItemNotAMethodInStruct, NoItemInStruct,
    StaticCallToMethodInStruct,
};
use scopes::{ScopeManager, VariableInfo};
use tracing::{debug, trace};

#[derive(Debug, Clone)]
pub struct GenericsCtx {
    /// Generics available in the current scope.
    pub generics: Vec<GenericParam>,
}

impl Default for GenericsCtx {
    fn default() -> Self {
        Self::new()
    }
}

impl GenericsCtx {
    pub fn new() -> Self {
        Self { generics: Vec::new() }
    }

    pub fn push_generic(&mut self, generic: GenericParam) {
        self.generics.push(generic);
    }

    pub fn clear_generics(&mut self) {
        self.generics.clear();
    }

    pub fn find(&self, name: &str) -> Option<&GenericParam> {
        self.generics.iter().find(|g| g.ident.to_string() == name)
    }
}

#[derive(Debug)]
pub struct NameResolver<'a> {
    pub ctx: TemporaryDiagnosticContext,
    pub map: ModuleMap,
    pub scopes: ScopeManager,
    pub file: usize,
    pub lints: &'static Lints,
    pub inside_comptime: bool,
    pub compiled: &'a mut CompiledModules,
    pub external: bool,
    pub simple: &'a mut SimpleModules,
    pub generics: GenericsCtx,
    pub current_sym: Option<GlobalSymbol>,
}

impl<'a> NameResolver<'a> {
    pub fn new(
        map: ModuleMap,
        lints: &'static Lints,
        compiled: &'a mut CompiledModules,
        simple: &'a mut SimpleModules,
    ) -> Self {
        Self {
            ctx: TemporaryDiagnosticContext::new(),
            map,
            scopes: ScopeManager::new(0),
            file: 0,
            lints,
            inside_comptime: false,
            compiled,
            external: false,
            simple,
            generics: GenericsCtx::new(),
            current_sym: None,
        }
    }

    pub fn resolve_names(&mut self) {
        let mut mods = vec![];
        for mut module in self.map.modules.clone() {
            debug!("Resolving names for module: {:?}", module.barrel.file_id);

            self.file = module.barrel.file_id;
            for item in &mut module.barrel.items {
                self.walk_item(item);
            }

            mods.push(module);
        }

        self.map.modules = mods;
    }

    /// declare_param doesn't check for duplicates, because that is already handled by the
    /// [`AstValidator`](crate::validator::AstValidator)
    fn declare_param(&mut self, name: &str, info: VariableInfo) {
        diag_opt!(self.ctx, self.scopes.declare_variable(name.to_string(), info, false));
    }

    fn declare_variable(&mut self, name: &str, info: VariableInfo) {
        diag_opt!(self.ctx, self.scopes.declare_variable(name.to_string(), info, true));
    }

    // Check if a variable is declared in any accessible scope
    pub fn is_variable_declared(&self, name: &str) -> Option<(Scope, &VariableInfo)> {
        self.scopes.resolve_variable(name)
    }

    pub fn validate_var_name(&mut self, name: &str, span: Span) {
        let snake = name.to_case(Case::Snake);
        if name != snake && !self.external {
            self.ctx.emit_lint(self.lints.variable_not_snake_case(
                name.to_string(),
                snake,
                (span, self.file),
            ));
        }
    }

    pub fn resolve_path(&mut self, idents: &[Ident], expr_id: ItemId, from_call: bool) {
        if idents.is_empty() {
            return;
        }

        let name = idents[0].to_string();
        let span = (idents[0].span, self.file);

        match self.compiled.symbols.resolve(&name, self.file) {
            Some(sym) => {
                let item = self.simple.get_item(sym.id.item_id).clone();
                self.resolve_ident(item.ident);

                if from_call {
                    match &item.kind {
                        ItemKind::Struct(_) | ItemKind::Enum(_) => {
                            self.compiled.assign_path(expr_id, item.id);
                        }
                        _ => {
                            self.ctx.emit_impl(InvalidReceiverForStaticAccess { span, name });
                        }
                    }
                } else if idents.len() > 1 {
                    let second_ident = &idents[1];
                    let second_name = second_ident.to_string();

                    match &item.kind {
                        ItemKind::Namespace(symbols) => {
                            if let Some(sym) = symbols.get(&second_name) {
                                self.compiled.assign_path(expr_id, sym.1.0);
                            } else {
                                self.ctx.emit_impl(NamespaceMissingSymbol {
                                    span: (second_ident.span, self.file),
                                    name,
                                    symbol: second_name,
                                });
                            }
                        }
                        ItemKind::Enum(e) => {
                            if let Some(variant) = e.find(second_ident) {
                                self.compiled.assign_path(expr_id, variant.id);
                                self.compiled.add_enum(variant.id, item.id);
                            } else {
                                self.ctx.emit_impl(NoVariantOrItemInEnum {
                                    span: (second_ident.span, self.file),
                                    name: second_name,
                                    enum_name: name,
                                });
                            }
                        }
                        _ => {
                            self.ctx.emit_impl(InvalidPathAccess { span, name });
                        }
                    }
                } else {
                    self.compiled.assign_path(expr_id, item.id);
                }
            }
            None => {
                if from_call {
                    self.ctx.emit_impl(InvalidReceiverForStaticAccess { span, name });
                } else {
                    self.ctx.emit_impl(InvalidPathAccess { span, name });
                }
            }
        }
    }

    fn resolve_type(&mut self, ty: &Ty) {
        match &ty.kind {
            TyKind::Ref(ty, _) | TyKind::Ptr(ty, _) => self.resolve_type(&ty),
            TyKind::Mut(ty) | TyKind::Const(ty) | TyKind::Option(ty) | TyKind::Vec(ty) => {
                self.resolve_type(&ty)
            }
            TyKind::Result(ok, err) => {
                self.resolve_type(&ok);
                self.resolve_type(&err);
            }
            TyKind::Ident { ident, generics } => {
                // try to resolve the identifier
                let ident = *ident;
                self.resolve_ident(ident);

                for generic in &generics.params {
                    self.resolve_type(&generic.ty);
                }
            }
            TyKind::Primitive(_) | TyKind::Err(_) => {}
        }
    }

    pub fn resolve_ident(&mut self, ident: Ident) {
        let sym = self.compiled.symbols.resolve(&ident.to_string(), self.file);

        if let Some(sym) = sym {
            if let Some(current) = self.current_sym.clone() {
                self.compiled.add_item_relation(current, sym);
            } else {
                trace!("Resolving identifier {} outside of an item context", ident);
            }
        } else if self.generics.find(&ident.to_string()).is_none() {
            self.ctx.emit_impl(IdentifierNotFound {
                span: (ident.span, self.file),
                name: ident.to_string(),
            });
        }
    }

    fn resolve_variable(&mut self, ident: &Ident) -> Option<(Scope, VariableInfo)> {
        let var_name = ident.name.to_string();
        let scope = self.is_variable_declared(&var_name);

        if let Some((scope, var)) = scope {
            if self.inside_comptime && !scope.inside_comptime {
                self.ctx.emit_impl(AccessOutsideComptime {
                    span: (ident.span, self.file),
                    name: var_name,
                    decl: (var.span, self.file),
                });

                None
            } else {
                Some((scope, var.clone()))
            }
        } else {
            if self.compiled.symbols.resolve(&var_name, self.file).is_none() {
                self.ctx.emit_impl(UndeclaredVariable {
                    span: (ident.span, self.file),
                    name: var_name.clone(),
                });
            }

            None
        }
    }

    fn set_id(&mut self, id: Option<GlobalSymbol>) {
        if let Some(id) = id {
            self.current_sym = Some(id);
        }
    }
}

impl AstWalker for NameResolver<'_> {
    fn visit_let(&mut self, let_stmt: &mut Let) {
        let name = let_stmt.ident.to_string();

        self.declare_variable(&name, VariableInfo {
            id: let_stmt.id,
            is_const: if let Some(ty) = &let_stmt.ty { ty.is_const() } else { false },
            span: let_stmt.span,
        });

        self.validate_var_name(&name, let_stmt.ident.span);

        if let Some(ty) = &mut let_stmt.ty {
            self.resolve_type(ty);
        }

        if let Some(expr) = &mut let_stmt.value {
            self.walk_expr(expr);
        }
    }

    fn visit_block(&mut self, block: &mut Block) {
        self.scopes.push_scope(self.file, self.inside_comptime);

        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }

        self.scopes.pop_scope();
    }

    fn visit_type_alias(&mut self, ta: &mut TypeAlias) {
        match &mut ta.ty {
            TypeAliasValue::Const(expr) => {
                self.walk_expr(expr);
            }
            TypeAliasValue::Ty(ty) => {
                self.resolve_type(ty);
            }
        }
    }

    fn visit_struct(&mut self, str: &mut Struct) {
        let s = self.compiled.symbols.resolve(&str.ident.to_string(), self.file);

        let parent_sym = self.current_sym.clone();
        self.set_id(s);

        for generics in str.generics.params.clone() {
            self.generics.push_generic(generics);
        }

        for field in &mut str.fields {
            self.resolve_type(&field.ty);
        }

        for item in &mut str.items {
            let struct_sym = self.current_sym.clone();
            self.visit_item(item);

            if let Some(struct_sym) = struct_sym {
                if let Some(item_sym) =
                    self.compiled.symbols.resolve(&item.ident.to_string(), self.file)
                {
                    self.compiled.add_item_relation(struct_sym, item_sym);
                }
            }
        }

        self.generics.clear_generics();
        self.current_sym = parent_sym;
    }

    fn visit_fn(&mut self, func: &mut FnDecl) {
        let name = func.sig.name.to_string();
        let parent_sym = self.current_sym.clone();

        // Check if the function is located lower like for example in a function or an enum.
        let sym = if let Some(sym) = self.current_sym.clone() {
            let item = self.simple.get_item(sym.id.item_id).clone();

            self.compiled.symbols.resolve(&format!("{}::{}", item.ident, name), self.file)
        } else {
            self.compiled.symbols.resolve(&name, self.file)
        };
        self.set_id(sym);

        for generic in func.generics.params.clone() {
            self.generics.push_generic(generic);
        }
        self.scopes = ScopeManager::new(self.file);

        let camel = name.to_case(Case::Camel);

        if name != camel && !self.external {
            self.ctx.emit_lint(self.lints.function_not_camel_case(
                name,
                camel,
                (func.sig.name.span, self.file),
            ));
        }

        for param in &func.sig.params {
            self.declare_param(&param.name.to_string(), VariableInfo {
                id: param.id,
                is_const: false,
                span: param.span,
            });

            self.validate_var_name(&param.name.to_string(), param.name.span);
            self.resolve_type(&param.ty);
        }

        if let Some(body) = &mut func.body {
            self.visit_block(body);
        }

        self.generics.clear_generics();
        self.current_sym = parent_sym;
    }

    fn walk_item(&mut self, item: &mut Item) {
        match &mut item.kind {
            ItemKind::Fn(func) => self.visit_fn(func),
            ItemKind::Use(use_stmt) => self.visit_use(use_stmt),
            ItemKind::Struct(str) => self.visit_struct(str),
            ItemKind::TypeAlias(type_alias) => self.visit_type_alias(type_alias),
            ItemKind::Module(_) => {}
            ItemKind::External(external) => {
                self.external = true;
                for item in &mut external.items {
                    match &mut item.kind {
                        ItemKind::Fn(func) => self.visit_fn(func),
                        ItemKind::TypeAlias(ty) => self.visit_type_alias(ty),
                        _ => unreachable!("not allowed"),
                    }
                }
                self.external = false;
            }
            ItemKind::Namespace(_) => unreachable!(),
            ItemKind::Enum(e) => {
                let en = self.compiled.symbols.resolve(&e.ident.to_string(), self.file);

                let parent_sym = self.current_sym.clone();
                self.set_id(en);

                for generics in e.generics.params.clone() {
                    self.generics.push_generic(generics);
                }

                for variant in &mut e.variants {
                    for field in &mut variant.fields {
                        self.resolve_type(&field.ty);
                    }
                }

                for item in &mut e.items {
                    let enum_sym = self.current_sym.clone();
                    self.visit_item(item);

                    if let Some(enum_sym) = enum_sym {
                        if let Some(item_sym) =
                            self.compiled.symbols.resolve(&item.ident.to_string(), self.file)
                        {
                            self.compiled.add_item_relation(enum_sym, item_sym);
                        }
                    }
                }

                self.generics.clear_generics();
                self.current_sym = parent_sym;
            }
        }
    }

    fn walk_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::Unary(_, operand) => self.walk_expr(operand),
            ExprKind::Field(idents) => {
                self.resolve_variable(idents.first().unwrap());
            }
            ExprKind::Index(base, index) => {
                self.walk_expr(base);
                self.walk_expr(index);
            }
            ExprKind::Literal(_) => {}
            ExprKind::Paren(inner) | ExprKind::Return(inner) => self.walk_expr(inner),
            ExprKind::Var(ident) => {
                if let Some(item) = self.compiled.symbols.resolve(&ident.to_string(), self.file) {
                    let item = self.simple.get_item(item.id.item_id);

                    match &item.kind {
                        ItemKind::TypeAlias(ty) => {
                            expr.kind = match &ty.ty {
                                TypeAliasValue::Ty(ty) => ExprKind::Type(Box::new(ty.clone())),
                                TypeAliasValue::Const(expr) => {
                                    ExprKind::Comptime(Box::new(expr.clone()))
                                }
                            };
                        }
                        _ => {
                            self.ctx.emit_impl(IdentifierNotFound {
                                span: (ident.span, self.file),
                                name: ident.to_string(),
                            });
                        }
                    }
                } else {
                    self.resolve_variable(ident);
                }
            }
            ExprKind::AssignOp(lhs, _, rhs)
            | ExprKind::Binary(lhs, _, rhs)
            | ExprKind::Assign(lhs, rhs) => {
                self.walk_expr(lhs);
                self.walk_expr(rhs);
            }
            ExprKind::Block(block) => self.visit_block(block),
            ExprKind::Call(func, args) => {
                let name = func.as_ident().unwrap().to_string();

                let func_sym = self.compiled.symbols.resolve(&name, self.file);

                if func_sym.is_none() {
                    self.ctx
                        .emit(Box::new(UndeclaredFunction { span: (func.span, self.file), name }));
                }

                for arg in args {
                    self.walk_expr(arg);
                }
            }
            ExprKind::Comptime(expr) => {
                self.inside_comptime = true;
                self.walk_expr(expr);
                self.inside_comptime = false;
            }
            ExprKind::Array(items) => {
                for item in items {
                    self.walk_expr(item);
                }
            }
            ExprKind::Builtin(ident, args) => {
                let name = ident.to_string();
                let func = BUILTIN_FUNCTIONS.get(&name);

                if func.is_none() {
                    self.ctx
                        .emit(Box::new(UndeclaredFunction { span: (ident.span, self.file), name }));
                }

                for arg in args {
                    self.walk_expr(arg);
                }
            }
            ExprKind::StructConstructor(ident, _, fields) => {
                let name = ident.to_string();
                let func_sym = self.compiled.symbols.resolve(&name, self.file);

                if func_sym.is_none() {
                    self.ctx
                        .emit(Box::new(UndeclaredStruct { span: (ident.span, self.file), name }));
                }

                for (_, field) in fields {
                    self.walk_expr(field);
                }
            }
            ExprKind::StaticAccess(ident, expr) => {
                self.resolve_path(ident, expr.id, true);

                let assigned = self.compiled.assigned_paths.get(&expr.id);
                if let Some(assigned) = assigned {
                    let str = self.simple.get_item(*assigned).clone();

                    if let ExprKind::Call(ident, args) = &mut expr.kind {
                        for arg in args {
                            self.walk_expr(arg);
                        }

                        let ident = *ident.as_ident().unwrap();

                        if let Some(item) = str.kind.as_struct() {
                            let item = item.find_item(ident);

                            if let Some(item) = item {
                                if let ItemKind::Fn(func) = &item.kind {
                                    if !func.is_static() {
                                        self.ctx.emit_impl(StaticCallToMethodInStruct {
                                            span: (ident.span, self.file),
                                            struct_name: str.ident.to_string(),
                                        });
                                    }
                                } else {
                                    self.ctx.emit_impl(ItemNotAMethodInStruct {
                                        span: (ident.span, self.file),
                                        name: ident.name.to_string(),
                                        struct_name: str.ident.to_string(),
                                    });
                                }
                            } else {
                                self.ctx.emit_impl(NoItemInStruct {
                                    span: (ident.span, self.file),
                                    name: ident.name.to_string(),
                                    struct_name: str.ident.to_string(),
                                });
                            }
                        } else if let Some(item) = str.kind.as_enum() {
                            if item.find(&ident).is_none() {
                                let res = item.find_item(&ident);
                                if res.is_none() {
                                    self.ctx.emit_impl(NoVariantOrItemInEnum {
                                        span: (ident.span, self.file),
                                        name: ident.name.to_string(),
                                        enum_name: str.ident.to_string(),
                                    });
                                }
                            }
                        }
                    } else {
                        todo!("error message")
                    }
                }
            }
            ExprKind::MethodCall(_, call) => {
                // Identifiers can only be checked in type inference so we actually
                // know where to look for the method

                if let ExprKind::Call(_, args) = call.kind.clone() {
                    for mut arg in args {
                        self.walk_expr(&mut arg);
                    }
                }
            }
            ExprKind::Match(mt) => self.visit_match(mt),
            ExprKind::Path(idents) => {
                self.resolve_path(idents, expr.id, false);
            }
            ExprKind::Type(ty) => {
                self.resolve_type(&ty);
            }
            ExprKind::Unwrap(expr) => {
                self.walk_expr(expr);
            }
            ExprKind::Ternary(cond, then, else_) => {
                self.walk_expr(cond);
                self.walk_expr(then);
                self.walk_expr(else_);
            }
        }
    }
}

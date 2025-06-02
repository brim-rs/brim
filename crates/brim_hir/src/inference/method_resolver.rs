use crate::{
    MainContext,
    expr::HirExpr,
    inference::{
        errors::{
            CannotAccessFieldOnNonStruct, FailedToResolveItem, NoMethod, NoVariableForMethodAccess,
        },
        scope::{TypeInfo, TypeScopeManager},
    },
    items::HirItem,
    ty::HirTyKind,
};
use brim_middle::{ModuleId, temp_diag::TemporaryDiagnosticContext};
use brim_span::span::Span;

#[derive(Debug)]
pub struct MethodResolver<'a> {
    pub scope_manager: TypeScopeManager,
    pub temp: TemporaryDiagnosticContext,
    pub current_mod: ModuleId,
    pub main_ctx: &'a mut MainContext,
    pub idents: Vec<HirExpr>,
    pub processed: u16,
    pub all: u16,
}

impl<'a> MethodResolver<'a> {
    pub fn new(
        scope_manager: TypeScopeManager,
        current_mod: ModuleId,
        main_ctx: &'a mut MainContext,
        idents: Vec<HirExpr>,
    ) -> Self {
        Self {
            scope_manager,
            current_mod,
            main_ctx,
            temp: TemporaryDiagnosticContext::new(),
            idents: idents.clone(),
            processed: 0,
            all: idents.len() as u16,
        }
    }

    pub fn resolve_method_call(&mut self) -> Option<HirItem> {
        if self.processed >= self.all {
            return None;
        }

        let mut item = self.idents[self.processed as usize].clone().as_member_part();

        if self.processed == 0 {
            return if let Some(var) =
                self.scope_manager.resolve_variable(&item.to_string()).cloned()
            {
                self.processed += 1;
                self.resolve_from_type(&var.ty)
            } else {
                self.temp.emit_impl(NoVariableForMethodAccess {
                    span: (item.span, self.current_mod.into()),
                    name: item.to_string(),
                });
                None
            };
        }
        return None;
    }

    pub fn resolve_from_type(&mut self, ty: &HirTyKind) -> Option<HirItem> {
        let ident = ty.is_ident();
        assert!(ident.is_some(), "Expected type to be an identifier");
        let ident = ident.unwrap();

        let item =
            self.main_ctx.resolve_symbol(&ident.to_string(), self.current_mod.as_usize()).cloned();

        if let Some(item) = item {
            if self.processed >= self.all {
                Some(item)
            } else {
                let name = self.idents[self.processed as usize].as_member_part();
                if let Some(item) = item.as_struct() {
                    let field = item.get_field(&name.to_string());

                    if let Some(field) = field {
                        self.processed += 1;
                        self.resolve_from_type(&field.ty)
                    } else {
                        self.temp.emit_impl(NoMethod {
                            span: (name.span, self.current_mod.into()),
                            method: name.to_string(),
                            ty: ty.clone(),
                        });
                        None
                    }
                } else {
                    self.temp.emit_impl(CannotAccessFieldOnNonStruct {
                        span: (ident.span, self.current_mod.into()),
                        ty: ty.clone(),
                    });
                    return None;
                }
            }
        } else {
            self.temp.emit_impl(FailedToResolveItem {
                span: (ident.span, self.current_mod.into()),
                name: ident.to_string(),
            });
            return None;
        }
    }
}

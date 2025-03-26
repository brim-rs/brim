use crate::{
    Codegen,
    errors::BuiltinCastExpectedType,
    expr::{HirExpr, HirExprKind},
    ty::HirTyKind,
};
use brim_ast::{
    ItemId,
    token::{Lit, LitKind},
    ty::PrimitiveType,
};
use brim_diagnostics::diagnostic::ToDiagnostic;
use brim_span::symbols::Symbol;

pub type BuiltinResult<T> = Result<T, Box<dyn ToDiagnostic>>;

#[derive(Debug, Clone)]
pub struct BuiltInFunction {
    pub func: fn(usize, &mut Vec<HirExpr>) -> BuiltinResult<HirExpr>,
    pub codegen: Option<fn(&mut dyn Codegen, &mut Vec<HirExpr>) -> String>,
}

#[macro_export]
macro_rules! builtin_function {
    (fn $name:ident($file:ident $(, $($arg:ident),* $(, ...$rest:ident)?)?) {$($body:tt)*}) => {
        #[allow(unused_mut, unused_variables)]
        pub fn $name() -> BuiltInFunction {
            fn inner(file_param: usize, args: &mut Vec<HirExpr>) -> BuiltinResult<HirExpr> {
                let mut iter = args.iter_mut();
                $($(let mut $arg = iter.next().unwrap();)*)?
                $($(let $rest = iter.collect::<Vec<_>>();)?)?

                let $file = file_param;

                $($body)*
            }

            BuiltInFunction {
                func: inner,
                codegen: None,
            }
        }
    };
    (fn $name:ident($file:ident $(, $($arg:ident),* $(, ...$rest:ident)?)?) {$($body:tt)*}
        codegen($cg_ctx:ident) {$($cg_body:tt)*}) => {
        #[allow(unused_mut, unused_variables)]
        pub fn $name() -> BuiltInFunction {
            fn inner(file_param: usize, args: &mut Vec<HirExpr>) -> BuiltinResult<HirExpr> {
                let mut iter = args.iter_mut();
                $($(let mut $arg = iter.next().unwrap();)*)?
                $($(let $rest = iter.collect::<Vec<_>>();)?)?

                let $file = file_param;

                $($body)*
            }

            fn codegen_inner($cg_ctx: &mut dyn Codegen, args: &mut Vec<HirExpr>) -> String {
                let mut iter = args.iter_mut();
                $($(let mut $arg = iter.next().unwrap();)*)?
                $($(let $rest = iter.collect::<Vec<_>>();)?)?

                $($cg_body)*
            }

            BuiltInFunction {
                func: inner,
                codegen: Some(codegen_inner),
            }
        }
    };
}

builtin_function! {
    fn os(file) {
        Ok(HirExpr {
            id: ItemId::new(),
            ty: HirTyKind::Primitive(PrimitiveType::String),
            kind: HirExprKind::Literal(Lit::new(LitKind::Str, Symbol::new(&if cfg!(target_os = "linux") {
                "linux"
            } else if cfg!(target_os = "macos") {
                "macos"
            } else if cfg!(target_os = "windows") {
                "windows"
            } else {
                panic!("Unsupported operating system")
            }), None)),
            span: Default::default(),
        })
    }
}

builtin_function! {
    fn any_cast(file, a, b) {
        if let HirExprKind::Type(ty) = &b.kind {
            a.ty = ty.clone();
            Ok(a.clone())
        } else {
            Err(Box::new(BuiltinCastExpectedType {
                span: (b.span, file)
            }))
        }
    }
    codegen(cg_ctx) {
        format!("safe_cast<{}>({})", cg_ctx.generate_expr(b.clone()), cg_ctx.generate_expr(a.clone()))
    }
}

builtin_function! {
    fn cast(file, a, b) {
        if let HirExprKind::Type(ty) = &b.kind {
            a.ty = ty.clone();
            Ok(a.clone())
        } else {
            Err(Box::new(BuiltinCastExpectedType {
                span: (b.span, file)
            }))
        }
    }
    codegen(cg_ctx) {
        format!("static_cast<{}>({})", cg_ctx.generate_expr(b.clone()), cg_ctx.generate_expr(a.clone()))
    }
}

pub fn get_builtin_function(name: &str) -> Option<BuiltInFunction> {
    match name {
        "os" => Some(os()),
        "anyCast" => Some(any_cast()),
        "cast" => Some(cast()),
        _ => None,
    }
}

use crate::{expr::HirExpr, transformer::Transformer, ty::HirTyKind};

#[derive(Debug, Clone)]
pub struct BuiltInFunction {
    pub func: fn(&mut Transformer, Vec<HirExpr>) -> HirExpr,
}

#[macro_export]
macro_rules! builtin_function {
    (fn $name:ident($($arg:ident),* $(, ...$rest:ident)?) {$($body:tt)*}) => {
        #[allow(unused_mut, unused_variables)]
        pub fn $name() -> BuiltInFunction {
            fn inner(transformer: &mut Transformer, args: Vec<HirExpr>) -> HirExpr {
                let mut iter = args.into_iter();
                $(let mut $arg = iter.next().unwrap();)*
                $(let $rest = iter.collect::<Vec<_>>();)?

                $($body)*
            }

            BuiltInFunction {
                func: inner,
            }
        }
    };
}

builtin_function! {
    fn ok(value) {
        value.ty = HirTyKind::ResOk(Box::new(value.ty.clone()));

        value.clone()
    }
}

builtin_function! {
    fn err(value) {
        value.ty = HirTyKind::ResErr(Box::new(value.ty.clone()));

        value.clone()
    }
}

pub fn get_builtin_function(name: &str) -> Option<BuiltInFunction> {
    match name {
        "ok" => Some(ok()),
        "err" => Some(err()),
        _ => None,
    }
}

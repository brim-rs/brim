use crate::{
    Codegen, CompiledModules,
    expr::{HirExpr, HirExprKind},
    items::{HirItem, HirItemKind},
    stmts::{HirStmt, HirStmtKind},
    transformer::{HirModule, HirModuleMap, StoredHirItem},
    ty::HirTyKind,
};
use brim_ast::{
    ItemId,
    token::{Lit, LitKind},
    ty::PrimitiveType,
};
use brim_span::symbols::Symbol;

#[derive(Debug, Clone)]
pub struct BuiltInFunction {
    pub func: fn(&mut Vec<HirExpr>) -> HirExpr,
    pub codegen: Option<fn(&mut dyn Codegen, &mut Vec<HirExpr>) -> String>,
}

#[macro_export]
macro_rules! builtin_function {
        (fn $name:ident($($arg:ident),* $(, ...$rest:ident)?) {$($body:tt)*}) => {
            #[allow(unused_mut, unused_variables)]
            pub fn $name() -> BuiltInFunction {
                fn inner(args: &mut Vec<HirExpr>) -> HirExpr {
                    let mut iter = args.iter_mut();
                    $(let mut $arg = iter.next().unwrap();)*
                    $(let $rest = iter.collect::<Vec<_>>();)?

                    $($body)*
                }

                BuiltInFunction {
                    func: inner,
                    codegen: None,
                }
            }
        };
        (fn $name:ident($($arg:ident),* $(, ...$rest:ident)?) {$($body:tt)*}
            codegen($cg_ctx:ident) {$($cg_body:tt)*}) => {
            #[allow(unused_mut, unused_variables)]
            pub fn $name() -> BuiltInFunction {
                fn inner(args: &mut Vec<HirExpr>) -> HirExpr {
                    let mut iter = args.iter_mut();
                    $(let mut $arg = iter.next().unwrap();)*
                    $(let $rest = iter.collect::<Vec<_>>();)?

                    $($body)*
                }

                fn codegen_inner($cg_ctx: &mut dyn Codegen, args: &mut Vec<HirExpr>) -> String {
                    let mut iter = args.iter_mut();
                    $(let mut $arg = iter.next().unwrap();)*
                    $(let $rest = iter.collect::<Vec<_>>();)?

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
    fn os() {
        HirExpr {
            id: ItemId::dummy(),
            ty: HirTyKind::Primitive(PrimitiveType::Str),
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
        }
    }
}

pub fn get_builtin_function(name: &str) -> Option<BuiltInFunction> {
    match name {
        "os" => Some(os()),
        _ => None,
    }
}

use std::cmp::PartialEq;
use crate::compilation::code_gen::CodeGen;
use anyhow::Result;
use lazy_static::lazy_static;
use brim_cpp_compiler::CppBuild;
use crate::ast::expressions::CallExpr;
use crate::ast::types::TypeKind;
use crate::compilation::passes::type_checker::ResolvedType;

#[derive(Debug, PartialEq)]
pub enum BuiltInKind {
    Print,
    Ok,
    Err,
}

#[derive(Debug)]
pub struct BuiltIn {
    pub kind: BuiltInKind,
    pub source: Option<String>,
    pub needed_imports: Vec<String>,
    pub internal_name: String,
}


lazy_static! {
    pub static ref BUILT_INS: Vec<BuiltIn> = vec![
        BuiltIn {
            kind: BuiltInKind::Print,
            internal_name: "brim_builtin_print".to_string(),
            source: Some(include_str!("definitions/print.cpp").to_string()),
            needed_imports: vec!["iostream".to_string(), "string".to_string()],
        },
        BuiltIn {
            kind: BuiltInKind::Ok,
            internal_name: "brim_builtin_ok".to_string(),
            source: None,
            needed_imports: vec!["expected".to_string()],
        },
        BuiltIn {
            kind: BuiltInKind::Err,
            internal_name: "brim_builtin_err".to_string(),
            source: None,
            needed_imports: vec!["expected".to_string()],
        },
    ];
}

impl BuiltInKind {
    //                       typecheck?  return type  arguments
    pub fn signature(&self) -> (bool, ResolvedType, Vec<ResolvedType>) {
        match self {
            BuiltInKind::Print => (true, ResolvedType::base(TypeKind::Void), vec![ResolvedType::base(TypeKind::String)]),
            BuiltInKind::Ok => (false, ResolvedType::base(TypeKind::Void), vec![ResolvedType::base(TypeKind::Void)]),
            BuiltInKind::Err => (false, ResolvedType::base(TypeKind::Void), vec![ResolvedType::base(TypeKind::Void)]),
        }
    }

    pub fn get_builtin(name: &str) -> Option<BuiltInKind> {
        match name {
            "print" => Some(BuiltInKind::Print),
            "ok" => Some(BuiltInKind::Ok),
            "err" => Some(BuiltInKind::Err),
            _ => None,
        }
    }
}

impl<'a> CodeGen<'a> {
    pub fn generate_built_in(&mut self, call: CallExpr, build_cpp: &mut CppBuild) -> Result<()> {
        let def =
            BUILT_INS.iter()
                .find(|b| b.kind == BuiltInKind::get_builtin(&call.callee).unwrap())
                .unwrap();

        for import in &def.needed_imports {
            self.needed_imports.push(import.clone());
        }

        match def.kind {
            BuiltInKind::Print => {
                self.injects.push(def.source.clone().unwrap());
                self.write_line(format!("{}(", def.internal_name));

                for (i, arg) in call.args.iter().enumerate() {
                    let arg = self.unit.ast().query_expr(*arg).clone();
                    self.generate_expr(arg, build_cpp)?;

                    if i < call.args.len() - 1 {
                        self.write(", ");
                    }
                }

                self.write(")");
            }
            // While using expected, we can just return the value
            BuiltInKind::Ok => {
                let arg = self.unit.ast().query_expr(call.args[0]).clone();
                self.generate_expr(arg, build_cpp)?;                
            }
            BuiltInKind::Err => {
                let arg = self.unit.ast().query_expr(call.args[0]).clone();
        
                self.write_line("std::unexpected(");
                self.generate_expr(arg, build_cpp)?;
                self.write(")");
            }
        }

        Ok(())
    }
}
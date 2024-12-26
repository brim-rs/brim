use std::cmp::PartialEq;
use crate::compilation::code_gen::CodeGen;
use anyhow::Result;
use lazy_static::lazy_static;
use crate::ast::expressions::CallExpr;
use crate::ast::types::TypeKind;
use crate::compilation::passes::type_checker::ResolvedType;

#[derive(Debug, PartialEq)]
pub enum BuiltInKind {
    Print,
}

#[derive(Debug)]
pub struct BuiltIn {
    pub kind: BuiltInKind,
    pub source: String,
    pub needed_imports: Vec<String>,
    pub internal_name: String,
}


lazy_static! {
    pub static ref BUILT_INS: Vec<BuiltIn> = vec![
        BuiltIn {
            kind: BuiltInKind::Print,
            internal_name: "__builtin_print".to_string(),
            source: include_str!("definitions/print.cpp").to_string(),
            needed_imports: vec!["iostream".to_string(), "string".to_string()],
        },
    ];
}

impl BuiltInKind {
    pub fn signature(&self) -> (ResolvedType, Vec<ResolvedType>) {
        match self {
            BuiltInKind::Print => (ResolvedType::base(TypeKind::Void), vec![ResolvedType::base(TypeKind::String)]),
        }
    }

    pub fn get_builtin(name: &str) -> Option<BuiltInKind> {
        match name {
            "print" => Some(BuiltInKind::Print),
            _ => None,
        }
    }
}

impl<'a> CodeGen<'a> {
    pub fn generate_built_in(&mut self, call: CallExpr) -> Result<()> {
        let def =
            BUILT_INS.iter()
                .find(|b| b.kind == BuiltInKind::get_builtin(&call.callee).unwrap())
                .unwrap();

        for import in &def.needed_imports {
            self.needed_imports.push(import.clone());
        }

        self.injects.push(def.source.clone());

        self.write_line(format!("{}(", def.internal_name));

        for (i, arg) in call.args.iter().enumerate() {
            let arg = self.unit.ast().query_expr(*arg).clone();
            self.generate_expr(arg)?;

            if i < call.args.len() - 1 {
                self.write(", ");
            }
        }

        self.write(")");

        Ok(())
    }
}
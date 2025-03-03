use crate::codegen::CppCodegen;
use brim_ast::{
    expr::BinOpKind,
    token::{Lit, LitKind},
};
use brim_hir::{
    builtin::get_builtin_function,
    expr::{HirExpr, HirExprKind, HirIfExpr, HirStructConstructor},
};
use std::fmt::Write;

impl CppCodegen {
    /// Generates C++ code for a given HIR expression
    pub fn generate_expr(&mut self, expr: HirExpr) -> String {
        // Check if this expression is expanded by a builtin function
        if let Some(fn_name) = self.hir().expanded_by_builtins.get(&expr.id).cloned() {
            let func = get_builtin_function(&fn_name).unwrap();
            self.hir_mut().expanded_by_builtins.remove(&expr.id);

            if let Some(codegen) = func.codegen {
                return (codegen)(self, &mut vec![expr.clone()]);
            }
        }

        self.generate_expr_kind(expr)
    }

    fn generate_expr_kind(&mut self, expr: HirExpr) -> String {
        match expr.kind {
            HirExprKind::Block(block) => {
                let mut code = String::new();
                for stmt in block.stmts {
                    code.push_str(&self.generate_stmt(stmt));
                }
                code
            }
            HirExprKind::Return(expr) => {
                let expr_code = self.generate_expr(*expr);
                format!("return {};", expr_code)
            }
            HirExprKind::Binary(lhs, op, rhs) => {
                let lhs_code = self.generate_expr(*lhs);
                let rhs_code = self.generate_expr(*rhs);

                // Special handling for power operator which doesn't exist in C++
                if op == BinOpKind::Power {
                    format!("(std::pow({}, {}))", lhs_code, rhs_code)
                } else {
                    format!("({} {} {})", lhs_code, self.bin_op(op), rhs_code)
                }
            }
            HirExprKind::Var(ident) => ident.name.to_string(),
            HirExprKind::Call(func, args, _) => self.generate_call_expr(func, args),
            HirExprKind::Literal(lit) => self.generate_lit(lit),
            HirExprKind::Index(expr, index) => {
                let expr_code = self.generate_expr(*expr);
                let index_code = self.generate_expr(*index);
                format!("{}[{}]", expr_code, index_code)
            }
            HirExprKind::Field(expr, field) => {
                let expr_code = self.generate_expr(*expr);
                format!("{}.{}", expr_code, field)
            }
            HirExprKind::If(if_stmt) => self.generate_if_expr(if_stmt),
            HirExprKind::Array(exprs) => self.generate_array_expr(exprs),
            HirExprKind::StructConstructor(str) => self.generate_struct_constructor(str),
            _ => panic!("Unsupported expression: {:?}", expr.kind),
        }
    }

    fn generate_call_expr(&mut self, func: Box<HirExpr>, args: Vec<HirExpr>) -> String {
        let fn_ident = func.as_ident().unwrap();
        let fn_name = fn_ident.to_string();

        let func_symbol = self
            .hir()
            .symbols
            .resolve(&fn_name, self.current_mod.as_usize())
            .unwrap();

        let func_mod_id = func_symbol.id.mod_id;

        let args_code = args
            .iter()
            .map(|arg| self.generate_expr(arg.clone()))
            .collect::<Vec<String>>()
            .join(", ");

        format!(
            "(module{}::{}({}))",
            func_mod_id.as_usize(),
            fn_name,
            args_code
        )
    }

    fn generate_if_expr(&mut self, if_stmt: HirIfExpr) -> String {
        let condition = self.generate_expr(*if_stmt.condition);
        let then_block = self.generate_expr(*if_stmt.then_block);

        let mut else_ifs = String::new();
        for branch in &if_stmt.else_ifs {
            let branch_condition = self.generate_expr(*branch.condition.clone());
            let branch_block = self.generate_expr(*branch.block.clone());
            write!(
                else_ifs,
                " else if ({}) {{ {} }}",
                branch_condition, branch_block
            )
            .unwrap();
        }

        let else_block = if let Some(else_block) = if_stmt.else_block {
            format!(" else {{ {} }}", self.generate_expr(*else_block))
        } else {
            String::new()
        };

        format!(
            "if ({}) {{ {} }}{}{}",
            condition, then_block, else_ifs, else_block
        )
    }

    fn generate_array_expr(&mut self, exprs: Vec<HirExpr>) -> String {
        let exprs_code = exprs
            .iter()
            .map(|expr| self.generate_expr(expr.clone()))
            .collect::<Vec<String>>()
            .join(", ");

        format!("{{ {} }}", exprs_code)
    }

    fn generate_struct_constructor(&mut self, str: HirStructConstructor) -> String {
        let ident = str.name.clone();

        let symbol = self
            .hir()
            .symbols
            .resolve(&ident.to_string(), self.current_mod.as_usize())
            .unwrap();

        let mod_id = symbol.id.mod_id;

        let generics = if str.generics.params.is_empty() {
            String::new()
        } else {
            format!("<{}>", self.generate_generic_args(&str.generics))
        };

        let mut code = format!("module{}::{}{}", mod_id.as_usize(), ident.name, generics);

        if !str.fields.is_empty() {
            let fields_code = str
                .fields
                .iter()
                .map(|(field, expr)| {
                    let expr_code = self.generate_expr(expr.clone());
                    format!(".{} = {}", field.name, expr_code)
                })
                .collect::<Vec<String>>()
                .join(", ");

            write!(code, " {{ {} }}", fields_code).unwrap();
        }

        code
    }

    pub fn generate_suffix(&self, suffix: String) -> String {
        match suffix.as_str() {
            "f32" => "f",
            "f64" => "",
            "i8" => "",
            "i16" => "",
            "i32" => "",
            "i64" => "L",
            "u8" => "u",
            "u16" => "u",
            "u32" => "u",
            "u64" => "uL",
            _ => &suffix,
        }
        .to_string()
    }

    pub fn generate_lit(&self, lit: Lit) -> String {
        match lit.kind {
            LitKind::Integer | LitKind::Float => {
                if let Some(suffix) = lit.suffix {
                    format!("{}{}", lit.symbol, self.generate_suffix(suffix.to_string()))
                } else {
                    lit.symbol.to_string()
                }
            }
            LitKind::Str => format!("\"{}\"", escape_string(&lit.symbol.to_string())),
            LitKind::Char => format!("'{}'", escape_char(&lit.symbol.to_string())),
            LitKind::Bool => lit.symbol.to_string(),
            LitKind::Byte => format!(
                "static_cast<unsigned char>('{}')",
                escape_char(&lit.symbol.to_string())
            ),
            LitKind::ByteStr => format!("\"{}\"", escape_string(&lit.symbol.to_string())),
            LitKind::CStr => format!("\"{}\"", escape_string(&lit.symbol.to_string())),
            _ => format!("/* Unsupported literal: {:?} */", lit.kind),
        }
    }

    pub fn bin_op(&self, op: BinOpKind) -> &'static str {
        match op {
            BinOpKind::Plus => "+",
            BinOpKind::Minus => "-",
            BinOpKind::Multiply => "*",
            BinOpKind::Divide => "/",
            BinOpKind::Modulo => "%",
            BinOpKind::And => "&",
            BinOpKind::Or => "|",
            BinOpKind::EqEq => "==",
            BinOpKind::Ne => "!=",
            BinOpKind::Lt => "<",
            BinOpKind::Le => "<=",
            BinOpKind::Gt => ">",
            BinOpKind::Ge => ">=",
            BinOpKind::Power => "pow", // Special handling in binary expression
            BinOpKind::Caret => "^",
            BinOpKind::ShiftLeft => "<<",
            BinOpKind::ShiftRight => ">>",
            BinOpKind::AndAnd => "&&",
            BinOpKind::OrOr => "||",
        }
    }
}

fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('\"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

fn escape_char(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('\'', "\\'")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

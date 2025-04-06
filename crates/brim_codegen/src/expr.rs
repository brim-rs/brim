use crate::codegen::CppCodegen;
use brim_ast::{
    ItemId,
    expr::{BinOpKind, UnaryOp},
    token::{Lit, LitKind},
};
use brim_hir::{
    Codegen,
    builtin::get_builtin_function,
    expr::{HirExpr, HirExprKind, HirIfStmt, HirStructConstructor},
    ty::HirTyKind,
};
use brim_span::span::Span;
use std::fmt::Write;

impl CppCodegen {
    pub fn generate_expr(&mut self, expr: HirExpr) -> String {
        if let Some(fn_name) = self.hir().expanded_by_builtins.get(&expr.id).cloned() {
            let func = get_builtin_function(&fn_name).unwrap();
            self.hir_mut().expanded_by_builtins.remove(&expr.id);
            let params = &mut self.hir().builtin_args.get(&expr.id).unwrap().clone();

            if let Some(codegen) = func.codegen {
                let string = (codegen)(self, params);
                return string;
            }
        }

        self.generate_expr_kind(expr)
    }

    fn generate_expr_kind(&mut self, expr: HirExpr) -> String {
        let code = match expr.kind {
            HirExprKind::Block(block) => {
                let mut code = String::new();
                for stmt in block.stmts {
                    let tabs = "  ".repeat(self.code.indent);
                    code.push_str(&format!("{}{}\n", tabs, self.generate_stmt(stmt)));
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
                } else if op == BinOpKind::OrElse {
                    format!("({} ? {}.value() : {})", lhs_code, lhs_code, rhs_code)
                } else {
                    format!("({} {} {})", lhs_code, self.bin_op(op), rhs_code)
                }
            }
            HirExprKind::Var(ident) => format!("brim_{}", ident),
            HirExprKind::Call(func, args, _) => self.generate_call_expr(func, args),
            HirExprKind::Literal(lit) => self.generate_lit(lit, expr.ty),
            HirExprKind::Index(expr, index) => {
                let expr_code = self.generate_expr(*expr);
                let index_code = self.generate_expr(*index);
                format!("{}[{}]", expr_code, index_code)
            }
            HirExprKind::Field(idents) => {
                let mut vals = idents
                    .iter()
                    .map(|id| format!("{}", id))
                    .collect::<Vec<String>>();
                if let Some(last) = idents.last() {
                    let last = last.clone();
                    vals.pop();

                    if last.to_string() == "len" {
                        vals.push("size()".to_string());
                    }

                    if last.to_string() == "ptr" {
                        vals.push("data()".to_string());
                    }
                };

                format!("brim_{}", vals.join("."),)
            }
            HirExprKind::If(ref if_stmt) => self.generate_if_stmt(if_stmt.clone()),
            HirExprKind::Array(exprs) => self.generate_array_expr(exprs),
            HirExprKind::StructConstructor(str) => self.generate_struct_constructor(str),
            HirExprKind::Type(ty) => self.generate_ty(ty),
            HirExprKind::Assign(lhs, rhs) => {
                let lhs_code = self.generate_expr(*lhs);
                let rhs_code = self.generate_expr(*rhs);
                format!("{} = {};", lhs_code, rhs_code)
            }
            HirExprKind::Unary(op, expr) => {
                let expr_code = self.generate_expr(*expr);
                match op {
                    UnaryOp::Minus => format!("-{}", expr_code),
                    UnaryOp::Not => format!("!{}", expr_code),
                    UnaryOp::Deref => format!("*{}", expr_code),
                    UnaryOp::Ref => format!("&{}", expr_code),
                    _ => unimplemented!(),
                }
            }
            HirExprKind::StaticAccess(id, expr) => {
                let item = self.compiled.get_item(id).clone();

                match expr.kind {
                    HirExprKind::Call(ident, args, _) => {
                        let ident = ident.as_ident().unwrap().to_string();
                        let call = format!("{}({})", ident, self.generate_call_args(args));
                        format!(
                            "(module{}::brim_{}_{})",
                            item.mod_id.as_usize(),
                            item.ident,
                            call
                        )
                    }
                    _ => unimplemented!(),
                }
            }
            HirExprKind::MethodCall(mut r_ident, call) => match call.kind.clone() {
                HirExprKind::Call(ident, args, _) => {
                    let last = r_ident.last().unwrap().clone();
                    r_ident.pop();
                    let path = r_ident
                        .iter()
                        .map(|id| id.to_string())
                        .collect::<Vec<String>>()
                        .join(".");

                    let call =
                        format!("{}(brim_{}, {})", last, path, self.generate_call_args(args));

                    format!("(brim_{}.brim_{})", path, call)
                }
                _ => unimplemented!(),
            },
            HirExprKind::Unwrap(expr) => {
                let expr_code = self.generate_expr(*expr);
                format!("unwrap({})", expr_code)
            }
            HirExprKind::Dummy => match expr.ty {
                HirTyKind::None => "std::nullopt".to_string(),
                _ => unreachable!(),
            },
            HirExprKind::Ternary(cond, then_block, else_block) => {
                let cond_code = self.generate_expr(*cond);
                let then_code = self.generate_expr(*then_block);
                let else_code = self.generate_expr(*else_block);

                format!("({} ? {} : {})", cond_code, then_code, else_code)
            }
            _ => panic!("Unsupported expression: {:?}", expr.kind),
        };

        code
    }

    fn generate_call_args(&mut self, args: Vec<HirExpr>) -> String {
        args.iter()
            .map(|arg| self.generate_expr(arg.clone()))
            .collect::<Vec<String>>()
            .join(", ")
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

        format!(
            "(module{}::brim_{}({}))",
            func_mod_id.as_usize(),
            fn_name,
            self.generate_call_args(args)
        )
    }

    pub fn generate_if_stmt(&mut self, if_stmt: HirIfStmt) -> String {
        let condition = self.generate_expr(*if_stmt.condition);
        let then_block = self.generate_expr(*if_stmt.then_block.clone());

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
            condition, then_block, else_ifs, else_block,
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

        let mut code = format!(
            "module{}::brim_{}{}",
            mod_id.as_usize(),
            ident.name,
            generics
        );

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

    pub fn generate_lit(&mut self, lit: Lit, expr_ty: HirTyKind) -> String {
        match lit.kind {
            LitKind::Integer | LitKind::Float => {
                if let Some(suffix) = lit.suffix {
                    format!(
                        "{}({}{})",
                        self.generate_ty(expr_ty),
                        lit.symbol,
                        self.generate_suffix(suffix.to_string())
                    )
                } else {
                    let ty = self.generate_ty(expr_ty);
                    format!("{}({})", ty, lit.symbol)
                }
            }
            LitKind::Str => format!(
                "std::string(\"{}\")",
                escape_string(&lit.symbol.to_string())
            ),
            LitKind::Char => format!("'{}'", escape_char(&lit.symbol.to_string())),
            LitKind::Bool => lit.symbol.to_string(),
            LitKind::Byte => format!(
                "static_cast<unsigned char>('{}')",
                escape_char(&lit.symbol.to_string())
            ),
            LitKind::ByteStr => format!(
                "std::vector<uint8_t>{{{}}}",
                lit.symbol
                    .to_string()
                    .chars()
                    .map(|b| format!("\'{}\'", b))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
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
            _ => unreachable!(),
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

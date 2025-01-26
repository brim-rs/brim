use crate::codegen::CppCodegen;
use brim_ast::{
    expr::BinOpKind,
    token::{Lit, LitKind},
};
use brim_hir::expr::{HirExpr, HirExprKind};

impl CppCodegen {
    pub fn generate_expr(&mut self, expr: HirExpr) -> String {
        match expr.kind {
            HirExprKind::Block(block) => {
                let mut code = String::new();
                for stmt in block.stmts {
                    code.push_str(&self.generate_stmt(stmt));
                }
                code
            }
            HirExprKind::Return(expr) => {
                let expr = self.generate_expr(*expr);
                format!("return {};", expr)
            }
            HirExprKind::Binary(lhs, op, rhs) => {
                let lhs = self.generate_expr(*lhs);
                let rhs = self.generate_expr(*rhs);
                format!("{} {} {}", lhs, self.bin_op(op), rhs)
            }
            HirExprKind::Var(ident) => ident.name.to_string(),
            HirExprKind::Call(func, args) => {
                let func = self.generate_expr(*func);
                let args = args
                    .iter()
                    .map(|arg| self.generate_expr(arg.clone()))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{}({})", func, args)
            }
            HirExprKind::Literal(lit) => self.generate_lit(lit),
            _ => String::new(),
        }
    }

    pub fn generate_lit(&self, lit: Lit) -> String {
        if let LitKind::Integer | LitKind::Float = lit.kind {
            if let Some(suffix) = lit.suffix {
                format!("{}{}", lit.symbol, suffix)
            } else {
                lit.symbol.to_string()
            }
        } else {
            match lit.kind {
                LitKind::Str => format!("\"{}\"", lit.symbol),
                LitKind::Char => format!("'{}'", lit.symbol),
                LitKind::Bool => lit.symbol.to_string(),
                LitKind::Byte => format!("b'{}'", lit.symbol),
                LitKind::ByteStr => format!("b\"{}\"", lit.symbol),
                LitKind::CStr => format!("\"{}\"", lit.symbol),
                _ => unreachable!(),
            }
        }
    }

    pub fn bin_op(&self, op: BinOpKind) -> &'static str {
        match op {
            BinOpKind::Plus => "+",
            BinOpKind::Minus => "-",
            BinOpKind::Multiply => "*",
            BinOpKind::Divide => "/",
            BinOpKind::Modulo => "%",
            BinOpKind::And => "&&",
            BinOpKind::Or => "||",
            BinOpKind::EqEq => "==",
            BinOpKind::Ne => "!=",
            BinOpKind::Lt => "<",
            BinOpKind::Le => "<=",
            BinOpKind::Gt => ">",
            BinOpKind::Ge => ">=",
            BinOpKind::Power => "**",
            BinOpKind::Caret => "^",
            BinOpKind::ShiftLeft => "<<",
            BinOpKind::ShiftRight => ">>",
            BinOpKind::AndAnd => "&&",
            BinOpKind::OrOr => "||",
        }
    }
}

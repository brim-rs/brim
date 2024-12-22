use anyhow::Result;
use crate::ast::expressions::{Expr, ExprKind, LiteralType};
use crate::compilation::code_gen::CodeGen;

impl<'a> CodeGen<'a> {
    pub fn generate_expr(&mut self, expr: Expr) -> Result<()> {
        match expr.kind {
            ExprKind::Literal(lit) => match lit.value {
                LiteralType::String(s) => self.write(format!("\"{}\"", s)),
                LiteralType::Char(c) => self.write(format!("'{}'", c)),
                LiteralType::Int(i) => self.write(i.to_string()),
                LiteralType::Float(f) => self.write(f.to_string()),
                LiteralType::Bool(b) => self.write(b.to_string()),
                LiteralType::Null => self.write("nullptr".to_string()),
            }
            ExprKind::Variable(var) => self.write(var.ident),
            _ => {}
        }

        Ok(())
    }
}
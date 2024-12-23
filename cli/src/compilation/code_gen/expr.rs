use anyhow::Result;
use crate::ast::expressions::{AccessKind, BinOpKind, Expr, ExprKind, LiteralType, UnOpKind};
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
            ExprKind::Parenthesized(expr) => {
                self.write("(");
                self.generate_expr(self.unit.ast().query_expr(expr.expr).clone())?;
                self.write(")");
            }
            ExprKind::Binary(bin) => {
                if matches!(bin.operator, BinOpKind::Power) {
                    self.needed_imports.push("cmath".to_string());

                    self.write("pow(");
                    self.generate_expr(self.unit.ast().query_expr(bin.left).clone())?;
                    self.write(", ");
                    self.generate_expr(self.unit.ast().query_expr(bin.right).clone())?;
                    self.write(")");

                    return Ok(());
                }

                if matches!(bin.operator, BinOpKind::Catch) {
                    todo!("Implement catch operator");

                    return Ok(());
                }

                self.generate_expr(self.unit.ast().query_expr(bin.left).clone())?;

                match bin.operator {
                    BinOpKind::Plus => self.write(" + "),
                    BinOpKind::Minus => self.write(" - "),
                    BinOpKind::Multiply => self.write(" * "),
                    BinOpKind::Divide => self.write(" / "),
                    BinOpKind::Modulo => self.write(" % "),
                    BinOpKind::And => self.write(" && "),
                    BinOpKind::Or => self.write(" || "),
                    BinOpKind::Equals => self.write(" == "),
                    BinOpKind::BangEquals => self.write(" != "),
                    BinOpKind::LessThan => self.write(" < "),
                    BinOpKind::LessThanOrEqual => self.write(" <= "),
                    BinOpKind::GreaterThan => self.write(" > "),
                    BinOpKind::GreaterThanOrEqual => self.write(" >= "),
                    BinOpKind::BitwiseAnd => self.write(" & "),
                    BinOpKind::BitwiseOr => self.write(" | "),
                    BinOpKind::BitwiseXor => self.write(" ^ "),
                    BinOpKind::ShiftLeft => self.write(" << "),
                    BinOpKind::ShiftRight => self.write(" >> "),
                    BinOpKind::EqualsEquals => self.write(" == "),
                    BinOpKind::Increment => self.write("++"),
                    BinOpKind::Decrement => self.write("--"),
                    _ => unreachable!()
                }

                self.generate_expr(self.unit.ast().query_expr(bin.right).clone())?;
            }
            ExprKind::Unary(unary) => {
                match unary.operator.kind {
                    UnOpKind::Minus => self.write("-"),
                    UnOpKind::LogicalNot => self.write("!"),
                    UnOpKind::BitwiseNot => self.write("~"),
                }

                self.generate_expr(self.unit.ast().query_expr(unary.expr).clone())?;
            }
            ExprKind::Access(access) => {
                self.generate_expr(self.unit.ast().query_expr(access.base).clone())?;
                
                match access.access {
                    AccessKind::Field(ident) => {
                        self.write(".");
                        self.generate_expr(self.unit.ast().query_expr(ident).clone())?
                    }
                    AccessKind::Index(index) => {
                        self.write("[");
                        self.generate_expr(self.unit.ast().query_expr(index).clone())?;
                        self.write("]");
                    }
                    AccessKind::StaticMethod(ident) => {
                        self.write("::");
                        self.generate_expr(self.unit.ast().query_expr(ident).clone())?;
                    }
                }
            }
            _ => {}
        }

        Ok(())
    }
}
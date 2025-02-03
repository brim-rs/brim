use crate::codegen::CppCodegen;
use brim_ast::{
    expr::BinOpKind,
    token::{Lit, LitKind},
};
use brim_hir::expr::{HirExpr, HirExprKind};

impl CppCodegen {
    pub fn generate_expr(&mut self, expr: HirExpr) -> String {
        let mut apply_paren = false;

        let code = match expr.kind {
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
                apply_paren = true;
                let lhs = self.generate_expr(*lhs);
                let rhs = self.generate_expr(*rhs);
                format!("{} {} {}", lhs, self.bin_op(op), rhs)
            }
            HirExprKind::Var(ident) => ident.name.to_string(),
            HirExprKind::Call(func, args) => {
                apply_paren = true;
                let fn_name = func.as_ident().unwrap().to_string();
                let func_mod_id = self
                    .hir
                    .resolve_symbol(&fn_name, self.current_mod)
                    .unwrap()
                    .mod_id;
                let args = args
                    .iter()
                    .map(|arg| self.generate_expr(arg.clone()))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!(
                    "{}::{}({})",
                    format!("module{}", func_mod_id.as_usize()),
                    fn_name,
                    args
                )
            }
            HirExprKind::Literal(lit) => self.generate_lit(lit),
            HirExprKind::Index(expr, index) => {
                let expr = self.generate_expr(*expr);
                let index = self.generate_expr(*index);
                format!("{}[{}]", expr, index)
            }
            HirExprKind::Field(expr, field) => {
                let expr = self.generate_expr(*expr);
                format!("{}.{}", expr, field)
            }
            // We use compound expressions to allow if statements to be expressions.
            // For example: `let x = if true { ... } else { ... };` (brim) -> `auto x = true ? ... : ...;` (C++)
            HirExprKind::If(if_stmt) => {
                let condition = self.generate_expr(*if_stmt.condition);
                let then_block = self.generate_expr(*if_stmt.then_block);

                let else_block = if let Some(else_block) = if_stmt.else_block {
                    format!("else {{ {} }}", self.generate_expr(*else_block))
                } else {
                    String::new()
                };

                let else_ifs = if_stmt
                    .else_ifs
                    .iter()
                    .map(|branch| {
                        format!(
                            "else if ({}) {{ {} }}",
                            self.generate_expr(*branch.condition.clone()),
                            self.generate_expr(*branch.block.clone())
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(" ");

                format!(
                    "if ({}) {{ {} }} {} {}",
                    condition, then_block, else_block, else_ifs
                )
            }
            HirExprKind::Array(exprs) => {
                let exprs = exprs
                    .iter()
                    .map(|expr| self.generate_expr(expr.clone()))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{{ {} }}", exprs)
            }
            _ => todo!("{:?}", expr.kind),
        };

        if apply_paren {
            format!("({})", code)
        } else {
            code
        }
    }

    pub fn generate_suffix(&self, suffix: &str) -> String {
        match suffix {
            "f32" => "f",
            "f64" => "",
            _ => suffix,
        }
        .to_string()
    }

    pub fn generate_lit(&self, lit: Lit) -> String {
        if let LitKind::Integer | LitKind::Float = lit.kind {
            if let Some(suffix) = lit.suffix {
                format!(
                    "{}{}",
                    lit.symbol,
                    self.generate_suffix(&suffix.to_string())
                )
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

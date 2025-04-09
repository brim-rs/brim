use crate::codegen::CppCodegen;
use brim_ast::expr::MatchArm;
use brim_hir::{
    Codegen,
    expr::{HirExprKind, HirMatchArm},
    stmts::{HirStmt, HirStmtKind},
};

impl CppCodegen {
    pub fn generate_stmt(&mut self, stmt: HirStmt) -> String {
        match stmt.kind {
            HirStmtKind::Expr(expr) => format!("{};", self.generate_expr(expr)),
            HirStmtKind::Let { value, ty, ident } => {
                let ty = self.generate_ty(ty.unwrap());

                let ident = ident.to_string();
                if let Some(value) = value {
                    if let HirExprKind::Ternary(cond, then, else_) = value.kind {
                        let var_decl = format!("{ty} brim_{ident};");

                        return format!(
                            "{var_decl}\n if ({}) {{ brim_{ident} = {}; }} else {{ brim_{ident} = {}; }}",
                            self.generate_expr(*cond),
                            self.generate_expr(*then),
                            self.generate_expr(*else_),
                        );
                    }

                    let value = self.generate_expr(value);
                    format!("{ty} brim_{ident} = {value};")
                } else {
                    format!("{ty} brim_{ident};")
                }
            }
            HirStmtKind::If(if_expr) => self.generate_if_stmt(if_expr),
            HirStmtKind::Match(mt) => {
                let expr = self.generate_expr(*mt.expr);
                let mut arms = vec![];
                let mut has_else = false;

                for (i, arm) in mt.arms.iter().enumerate() {
                    match arm.clone() {
                        HirMatchArm::Case(pat, block) => {
                            let pat = self.generate_expr(pat);
                            let block = self.generate_expr(block);
                            if i == 0 {
                                arms.push(format!("if ({pat} == {expr}) {{ {block} }}"));
                            } else {
                                arms.push(format!("else if ({pat} == {expr}) {{ {block} }}"));
                            }
                        }
                        HirMatchArm::Else(block) => {
                            let block = self.generate_expr(block);
                            arms.push(format!("else {{ {block} }}"));
                            has_else = true;
                        }
                    }
                }

                let arms = arms.join("\n");
                arms
            }
        }
    }
}

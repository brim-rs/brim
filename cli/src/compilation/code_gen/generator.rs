use crate::ast::item::TopLevelItem;
use crate::compilation::code_gen::CodeGen;
use anyhow::Result;
use crate::ast::statements::{Stmt, StmtKind};

impl<'a> CodeGen<'a> {
    pub fn generate_item(&mut self, item: TopLevelItem) -> Result<()> {
        let stmt = item.stmt;
        let stmt = self.unit.ast().query_stmt(stmt).clone();

        self.generate_stmt(stmt)?;

        Ok(())
    }

    pub fn generate_stmt(&mut self, stmt: Stmt) -> Result<()> {
        match stmt.kind {
            StmtKind::Fn(function) => self.generate_fn(function)?,
            StmtKind::Block(block) => {
                for stmt in block.stmts {
                    let stmt = self.unit.ast().query_stmt(stmt).clone();
                    self.generate_stmt(stmt)?;
                }
            }
            StmtKind::Expr(expr) => {
                let expr = self.unit.ast().query_expr(expr).clone();

                self.generate_expr(expr)?;
            }
            StmtKind::Let(let_stmt) => {
                let ident = let_stmt.ident.literal();

                let expr = self.unit.ast().query_expr(let_stmt.initializer).clone();
                let typ = self.map_type(if let Some(typ) = let_stmt.type_annotation {
                    Some(typ)
                } else {
                    Some(expr.ty.to_type_annotation())
                }, vec![]);

                self.write_before(format!("{} {} = ", typ, ident));
                self.generate_expr(expr)?;
                self.write(";\n");
            }
            StmtKind::Return(ret) => {
                if let Some(expr) = ret.expr {
                    let expr = self.unit.ast().query_expr(expr).clone();
                    self.write_before("return ");
                    self.generate_expr(expr)?;
                    self.write(";\n");
                } else {
                    self.write("return;\n");
                }
            }
            StmtKind::Struct(struct_def) => self.generate_struct_def(struct_def)?,
            StmtKind::Enum(enum_def) => self.generate_enum(enum_def)?,
            StmtKind::If(if_stmt) => {
                let condition = self.unit.ast().query_expr(if_stmt.condition).clone();
                let then_block = self.unit.ast().query_stmt(if_stmt.then_block).clone();

                self.write_before("if (");
                self.generate_expr(condition)?;
                self.write(") {\n");
                self.push_indent();
                self.generate_stmt(then_block)?;
                self.pop_indent();
                self.write_before("}");

                for else_if in &if_stmt.else_ifs {
                    self.write(" else if (");
                    let condition = self.unit.ast().query_expr(else_if.condition).clone();
                    self.generate_expr(condition)?;
                    self.write(") {\n");
                    self.push_indent();
                    let block = self.unit.ast().query_stmt(else_if.block).clone();
                    self.generate_stmt(block)?;
                    self.pop_indent();
                    self.write_before("}");
                }

                if let Some(else_block) = &if_stmt.else_block {
                    self.write(" else {\n");
                    self.push_indent();
                    let block = self.unit.ast().query_stmt(else_block.block).clone();
                    self.generate_stmt(block)?;
                    self.pop_indent();
                    self.write_line("}");
                }
            }
            _ => {}
        }

        Ok(())
    }
}
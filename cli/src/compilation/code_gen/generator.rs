use crate::{
    ast::{
        item::TopLevelItem,
        statements::{Stmt, StmtKind},
    },
    compilation::code_gen::CodeGen,
    context::GlobalContext,
};
use anyhow::Result;
use brim_cpp_compiler::CppBuild;

impl<'a> CodeGen<'a> {
    pub fn generate_item(
        &mut self,
        item: TopLevelItem,
        global: &mut GlobalContext,
        build_cpp: &mut CppBuild,
    ) -> Result<()> {
        let stmt = item.stmt;
        let stmt = self.unit.ast().query_stmt(stmt).clone();

        self.generate_stmt(stmt, global, build_cpp)?;

        Ok(())
    }

    pub fn generate_stmt(
        &mut self,
        stmt: Stmt,
        global: &mut GlobalContext,
        build_cpp: &mut CppBuild,
    ) -> Result<()> {
        match stmt.kind {
            StmtKind::Fn(function) => self.generate_fn(function, global, build_cpp)?,
            StmtKind::Block(block) => {
                for stmt in block.stmts {
                    let stmt = self.unit.ast().query_stmt(stmt).clone();
                    self.generate_stmt(stmt, global, build_cpp)?;
                }
            }
            StmtKind::Expr(expr) => {
                let expr = self.unit.ast().query_expr(expr).clone();

                self.generate_expr(expr, build_cpp)?;
            }
            StmtKind::Let(let_stmt) => {
                let ident = let_stmt.ident.literal();

                let expr = self.unit.ast().query_expr(let_stmt.initializer).clone();
                // let typ = self.map_type(
                //     if let Some(typ) = let_stmt.type_annotation {
                //         Some(typ)
                //     } else {
                //         Some(expr.ty.to_type_annotation())
                //     },
                //     vec![],
                // );

                // auto for now
                self.write_before(format!("auto {} = ", ident));
                self.generate_expr(expr, build_cpp)?;
                self.write("\n");
            }
            StmtKind::Return(ret) => {
                if let Some(expr) = ret.expr {
                    let expr = self.unit.ast().query_expr(expr).clone();
                    self.write_before("return ");
                    self.generate_expr(expr, build_cpp)?;
                    self.write("\n");
                } else {
                    self.write("return\n");
                }
            }
            StmtKind::Struct(struct_def) => self.generate_struct_def(struct_def)?,
            StmtKind::Enum(enum_def) => self.generate_enum(enum_def)?,
            StmtKind::If(if_stmt) => {
                let condition = self.unit.ast().query_expr(if_stmt.condition).clone();
                let then_block = self.unit.ast().query_stmt(if_stmt.then_block).clone();

                self.write_before("if (");
                self.generate_expr(condition, build_cpp)?;
                self.write(") {\n");
                self.push_indent();
                self.generate_stmt(then_block, global, build_cpp)?;
                self.pop_indent();
                self.write_before("}");

                for else_if in &if_stmt.else_ifs {
                    self.write(" else if (");
                    let condition = self.unit.ast().query_expr(else_if.condition).clone();
                    self.generate_expr(condition, build_cpp)?;
                    self.write(") {\n");
                    self.push_indent();
                    let block = self.unit.ast().query_stmt(else_if.block).clone();
                    self.generate_stmt(block, global, build_cpp)?;
                    self.pop_indent();
                    self.write_before("}");
                }

                if let Some(else_block) = &if_stmt.else_block {
                    self.write(" else {\n");
                    self.push_indent();
                    let block = self.unit.ast().query_stmt(else_block.block).clone();
                    self.generate_stmt(block, global, build_cpp)?;
                    self.pop_indent();
                    self.write_line("}");
                }
            }
            StmtKind::Use(use_stmt) => {
                let (_, mut unit) = self.loader.load_unit(&use_stmt.from.literal(), self.unit)?;

                let codegen =
                    &mut CodeGen::new(&mut unit, self.loader, self.build_type.clone(), false)?;
                codegen.generate_and_write(global, build_cpp)?;

                self.injects.push(String::from_utf8(codegen.buf.clone())?);
            }
            _ => {}
        }

        self.write(";");

        Ok(())
    }
}

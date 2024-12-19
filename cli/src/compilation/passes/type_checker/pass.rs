use std::collections::HashMap;
use std::sync::Arc;
use crate::compilation::passes::type_checker::ResolvedType;
use anyhow::Result;
use crate::ast::{ExprId, GetSpan, StmtId};
use crate::ast::expressions::{ExprKind, LiteralType, UnOpKind, Unary};
use crate::ast::statements::{StmtKind, TypeAnnotation};
use crate::ast::types::TypeKind;
use crate::compilation::imports::UnitLoader;
use crate::compilation::passes::Pass;
use crate::compilation::unit::CompilationUnit;
use crate::error::diagnostic::{Diagnostic, Diagnostics};
use crate::error::span::TextSpan;

#[derive(Debug)]
pub struct TypeChecker<'a> {
    pub scopes: Vec<HashMap<String, ResolvedType>>,
    pub unit: &'a mut CompilationUnit,
    pub loader: &'a mut UnitLoader,
    pub diags: &'a mut Diagnostics,
}

impl<'a> TypeChecker<'a> {
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn declare_variable(&mut self, name: String, typ: ResolvedType) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, typ);
        }
    }

    pub fn set_variable(&mut self, name: &str, val: ResolvedType, span: TextSpan) -> Result<()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), val);
                return Ok(());
            }
        }

        self.diags.new_diagnostic(Diagnostic::error(
            format!("Variable '{}' not found in scope", name),
            vec![(span, None)],
            vec![],
        ), Arc::new(self.unit.source.clone()));

        Ok(())
    }

    pub fn find_variable(&self, name: &str) -> Option<&ResolvedType> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }
}

impl<'a> TypeChecker<'a> {
    pub fn run(&mut self) -> Result<()> {
        let unit = &mut self.unit.clone();
        for item in unit.parser.ast.top_level_items.cloned_indices() {
            self.visit_item(unit, item)?;
        }

        Ok(())
    }

    pub fn resolve_from_expr(&mut self, expr: ExprId) -> Result<ResolvedType> {
        let expr = self.unit.ast().query_expr(expr).clone();

        let result = match expr.kind {
            ExprKind::Unary(unary) => self.validate_unary(unary)?,
            ExprKind::Literal(literal) => match literal.value {
                LiteralType::Null => ResolvedType::base(TypeKind::Null),
                LiteralType::Bool(_) => ResolvedType::base(TypeKind::Bool),
                LiteralType::Int(_) => ResolvedType::base(TypeKind::I32),  // ----]
                //      ] We default it to I32 or F32. Might have to change this later
                LiteralType::Float(_) => ResolvedType::base(TypeKind::F32), // ----]
                LiteralType::String(_) => ResolvedType::base(TypeKind::String),
                LiteralType::Char(_) => ResolvedType::base(TypeKind::Char),
            },
            ExprKind::Array(vec) => {
                let mut vec_type = ResolvedType::base(TypeKind::Null);
                for expr in &vec.exprs {
                    let expr_type = self.resolve_from_expr(expr.clone())?;
                    if vec_type.kind == TypeKind::Null {
                        vec_type = expr_type;
                    } else if !ResolvedType::matches(&vec_type, &expr_type) {
                        let expr = self.unit.ast().query_expr(expr.clone());

                        self.diags.new_diagnostic(Diagnostic::error(
                            format!("All elements of a vector must have the same type, expected '{}', found '{}'", vec_type, expr_type),
                            vec![(expr.span(
                                self.unit.ast()
                            ).clone(), None)],
                            vec![],
                        ), Arc::new(self.unit.source.clone()));
                    }
                }

                ResolvedType::base(TypeKind::Array(Box::new(vec_type)))
            }
            ExprKind::Variable(var) => {
                let literal = var.ident;
                if let Some(var_type) = self.find_variable(&literal.clone()) {
                    var_type.clone()
                } else {
                    self.diags.new_diagnostic(Diagnostic::error(
                        format!("Variable '{}' not found in scope", literal),
                        vec![(var.token.span.clone(), None)],
                        vec![],
                    ), Arc::new(self.unit.source.clone()));

                    ResolvedType::base(TypeKind::Null)
                }
            }
            _ => ResolvedType::base(TypeKind::Null)
        };

        Ok(result)
    }

    pub fn validate_unary(&mut self, unary: Unary) -> Result<ResolvedType> {
        match unary.operator.kind {
            UnOpKind::Minus | UnOpKind::BitwiseNot => {
                let expr_type = self.resolve_from_expr(unary.expr)?;

                if !expr_type.is_number() {
                    self.diags.new_diagnostic(Diagnostic::error(
                        format!("Unary expressions only accepts numbers and floats, found '{}'", expr_type),
                        vec![(unary.span(
                            self.unit.ast()
                        ), None)],
                        vec![],
                    ), Arc::new(self.unit.source.clone()));
                }

                Ok(ResolvedType::base(expr_type.kind))
            }
            UnOpKind::LogicalNot => {
                self.resolve_from_expr(unary.expr)?;
                Ok(ResolvedType::base(TypeKind::Bool))
            }
        }
    }
}

impl<'a> Pass for TypeChecker<'a> {
    fn do_visit_statement(&mut self, statement: StmtId) -> Result<()> {
        let statement = self.unit.ast().query_stmt(statement).clone();

        match statement.kind {
            StmtKind::Expr(expr) => {
                self.resolve_from_expr(expr)?;
            }
            StmtKind::Fn(function) => {
                for param in function.params.clone() {
                    self.declare_variable(param.ident.literal().clone(), ResolvedType::from_type_annotation(param.type_annotation.clone()));
                }

                if let Some(body_id) = function.body {
                    self.visit_statement(body_id)?;
                }
            }
            StmtKind::Block(block) => {
                self.enter_scope();
                for stmt in block.stmts.clone() {
                    self.visit_statement(stmt)?;
                }
                self.exit_scope();
            }
            StmtKind::Let(let_stmt) => {
                let typ = self.resolve_from_expr(let_stmt.initializer.clone())?;

                if let Some(expected) = let_stmt.type_annotation {
                    let expected = ResolvedType::from_type_annotation(expected);
                    if !ResolvedType::matches(&typ, &expected) {
                        let expr = self.unit.ast().query_expr(let_stmt.initializer.clone());

                        self.diags.new_diagnostic(Diagnostic::error(
                            format!("Expected type '{}', found '{}'", expected, typ),
                            vec![(expr.span(
                                self.unit.ast()
                            ).clone(), None)],
                            vec![],
                        ), Arc::new(self.unit.source.clone()));
                    }
                }

                self.declare_variable(let_stmt.ident.literal().clone(), typ);
            }
            StmtKind::While(while_stmt) => {
                let condition_type = self.resolve_from_expr(while_stmt.condition.clone())?;

                if condition_type.kind != TypeKind::Bool {
                    let expr = self.unit.ast().query_expr(while_stmt.condition.clone());

                    self.diags.new_diagnostic(Diagnostic::error(
                        format!("While condition must be a boolean, found '{}'", condition_type),
                        vec![(expr.span(
                            self.unit.ast()
                        ).clone(), Some("expected boolean".to_string()))],
                        vec![],
                    ), Arc::new(self.unit.source.clone()));
                }

                self.visit_statement(while_stmt.block)?;
            }
            StmtKind::Loop(loop_stmt) => {
                self.visit_statement(loop_stmt.block)?;
            }
            StmtKind::If(if_stmt) => {
                self.visit_statement(if_stmt.then_block)?;

                for else_if in if_stmt.else_ifs {
                    self.visit_statement(else_if.block)?;
                }

                if let Some(else_block) = if_stmt.else_block {
                    self.visit_statement(else_block.block)?;
                }
            }
            _ => {}
        }

        Ok(())
    }
}
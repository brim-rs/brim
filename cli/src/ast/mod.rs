mod statements;
mod expressions;

use crate::ast::expressions::Expr;
use crate::ast::statements::Stmt;
use crate::error::span::TextSpan;
use crate::idx::Idx;
use crate::idx;
use crate::idx::IdxVec;

idx!(StmtId);
idx!(ExprId);
idx!(ItemId);

#[cfg_attr(test, derive(Clone))]
#[derive(Debug)]
pub struct Ast {
    pub statements: IdxVec<StmtId, Stmt>,
    pub expressions: IdxVec<ExprId, Expr>,
}

impl Ast {
    pub fn query_expr(&self, expr_id: ExprId) -> &Expr {
        &self.expressions[expr_id]
    }

    fn query_expr_mut(&mut self, expr_id: ExprId) -> &mut Expr {
        &mut self.expressions[expr_id]
    }

    pub fn query_stmt(&self, stmt_id: StmtId) -> &Stmt {
        &self.statements[stmt_id]
    }

    fn query_stmt_mut(&mut self, stmt_id: StmtId) -> &mut Stmt {
        &mut self.statements[stmt_id]
    }
}

pub trait GetSpan {
    fn span(&self, ast: &Ast) -> TextSpan;
}
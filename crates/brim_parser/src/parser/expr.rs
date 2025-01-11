use brim::expr::{ConstExpr, Expr};
use brim::ty::Ty;
use crate::parser::{PResult, Parser};

impl<'a> Parser<'a> {
    pub fn parse_const_expr(&mut self) -> PResult<'a, ConstExpr> {
        todo!()
    }

    pub fn parse_expr(&mut self) -> PResult<'a, Expr> {
        todo!()
    }
}
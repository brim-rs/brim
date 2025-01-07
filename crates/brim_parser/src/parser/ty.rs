use crate::parser::PToken;
use crate::parser::PTokenKind;
use brim::{Const, NodeId};
use brim::span::Span;
use brim::token::{BinOpToken, TokenKind};
use brim::ty::{Const, Ty, TyKind};
use crate::parser::{PResult, Parser};
use crate::parser::errors::ConstAfter;
use crate::{debug_ident, ptok};

impl<'a> Parser<'a> {
    pub fn parse_type(&mut self) -> PResult<'a, Ty> {
        let span = self.current().span;

        let kind = if self.eat(TokenKind::BinOp(BinOpToken::And)) {
            self.parse_ref(false)?
        } else if self.eat(TokenKind::BinOp(BinOpToken::Star)) {
            self.parse_ptr(false)?
        } else if self.current().is_keyword(Const) {
            self.parse_const()?
        } else {
            todo!()
        };

        Ok(Ty {
            span,
            kind,
            id: NodeId::max(),
        })
    }

    pub fn parse_const(&mut self) -> PResult<'a, TyKind> {
        self.eat_keyword(ptok!(Const));

        if self.eat(TokenKind::BinOp(BinOpToken::And)) {
            Ok(self.parse_ref(true)?)
        } else if self.eat(TokenKind::BinOp(BinOpToken::Star)) {
            Ok(self.parse_ptr(true)?)
        } else {
            todo!()
        }
    }

    pub fn parse_ref(&mut self, constant: bool) -> PResult<'a, TyKind> {
        if self.current().is_keyword(Const) {
            self.emit(ConstAfter {
                span: (self.prev().span, self.file),
                before: "reference",
            });
        }

        let ty = self.parse_type()?;
        Ok(TyKind::Ref(Box::new(ty), Const::from_bool(constant)))
    }
    
    pub fn parse_ptr(&mut self, constant: bool) -> PResult<'a, TyKind> {
        if self.current().is_keyword(Const) {
            self.emit(ConstAfter {
                span: (self.prev().span, self.file),
                before: "pointer",
            });
        }

        let ty = self.parse_type()?;
        Ok(TyKind::Ptr(Box::new(ty), Const::from_bool(constant)))
    }
}
use brim_ast::{Const, NodeId};
use brim_ast::item::Ident;
use brim_ast::token::{BinOpToken, Delimiter, Orientation, TokenKind};
use brim_ast::ty::{Const, PrimitiveType, Ty, TyKind};
use crate::parser::PToken;
use crate::parser::PTokenKind;
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
        } else if self.current().is_delimiter(Delimiter::Bracket, Orientation::Open) {
            self.parse_array()?
        } else {
            let ident = self.parse_ident()?;

            if let Some(primitive) = self.is_primitive(ident)? {
                TyKind::Primitive(primitive)
            } else {
                TyKind::Ident {
                    ident,
                    generics: self.parse_generics()?,
                }
            }
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

    pub fn parse_array(&mut self) -> PResult<'a, TyKind> {
        self.expect_oparen()?;

        let ty = self.parse_type()?;
        self.expect(TokenKind::Semicolon)?;
        let size = self.parse_const_expr()?;

        self.expect_cparen()?;
        Ok(TyKind::Array(Box::new(ty), size))
    }

    pub fn is_primitive(&mut self, ident: Ident) -> PResult<'a, Option<PrimitiveType>> {
        Ok(PrimitiveType::try_from_ident(ident))
    }
}
use crate::{
    parser::{PResult, PToken, PTokenKind, Parser, errors::ConstAfter},
    ptok,
};
use brim_ast::{
    Const,
    item::Ident,
    token::{BinOpToken, Delimiter, Orientation, TokenKind},
    ty::{Const, PrimitiveType, Ty, TyKind},
};

impl Parser {
    pub fn parse_type(&mut self) -> PResult<Ty> {
        let span = self.current().span;

        let kind = if self.eat(TokenKind::BinOp(BinOpToken::And)) {
            self.parse_ref(false)?
        } else if self.eat(TokenKind::BinOp(BinOpToken::Star)) {
            self.parse_ptr(false)?
        } else if self.current().is_keyword(Const) {
            self.parse_const()?
        } else if self
            .current()
            .is_delimiter(Delimiter::Bracket, Orientation::Open)
        {
            self.parse_array()?
        } else {
            let ident = self.parse_ident()?;

            if let Some(primitive) = self.is_primitive(ident)? {
                TyKind::Primitive(primitive)
            } else {
                TyKind::Ident {
                    ident,
                    generics: self.parse_argument_generics()?,
                }
            }
        };

        let ty = Ty {
            span,
            kind,
            id: self.new_id(),
        };

        if self.eat(TokenKind::Bang) {
            return Ok(Ty {
                span,
                kind: TyKind::Result(Box::new(ty), Box::new(self.parse_type()?)),
                id: self.new_id(),
            });
        }

        Ok(ty)
    }

    pub fn parse_const(&mut self) -> PResult<TyKind> {
        self.eat_keyword(ptok!(Const));

        if self.eat(TokenKind::BinOp(BinOpToken::And)) {
            Ok(self.parse_ref(true)?)
        } else if self.eat(TokenKind::BinOp(BinOpToken::Star)) {
            Ok(self.parse_ptr(true)?)
        } else {
            Ok(TyKind::Const(Box::new(self.parse_type()?)))
        }
    }

    pub fn parse_ref(&mut self, constant: bool) -> PResult<TyKind> {
        if self.current().is_keyword(Const) {
            self.emit(ConstAfter {
                span: (self.prev().span, self.file),
                before: "reference",
            });
        }

        let ty = self.parse_type()?;
        Ok(TyKind::Ref(Box::new(ty), Const::from_bool(constant)))
    }

    pub fn parse_ptr(&mut self, constant: bool) -> PResult<TyKind> {
        if self.current().is_keyword(Const) {
            self.emit(ConstAfter {
                span: (self.prev().span, self.file),
                before: "pointer",
            });
        }

        let ty = self.parse_type()?;
        Ok(TyKind::Ptr(Box::new(ty), Const::from_bool(constant)))
    }

    pub fn parse_array(&mut self) -> PResult<TyKind> {
        self.expect_obracket()?;

        let ty = self.parse_type()?;
        self.expect(TokenKind::Semicolon)?;
        let size = self.parse_expr()?;

        self.expect_cbracket()?;
        Ok(TyKind::Array(Box::new(ty), size))
    }

    pub fn is_primitive(&mut self, ident: Ident) -> PResult<Option<PrimitiveType>> {
        Ok(PrimitiveType::try_from_string(ident.to_string()))
    }
}

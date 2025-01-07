use crate::parser::PToken;
use crate::parser::PTokenKind;
use brim::item::{GenericKind, GenericParam, Generics};
use brim::{Const, NodeId};
use brim::token::TokenKind;
use crate::parser::{PResult, Parser};
use crate::parser::errors::ExpectedClosingGenerics;
use crate::ptok;

impl<'a> Parser<'a> {
    pub fn parse_generics(&mut self) -> PResult<'a, Generics> {
        let token = self.current().span;
        if !self.eat(TokenKind::Lt) {
            // no generics. return early
            return Ok(Generics {
                span: self.prev().span.from_end(),
                params: vec![],
            });
        }

        let params = self.parse_generics_params()?;

        if !self.eat(TokenKind::Gt) {
            let expected_span = self.prev().span.from_end();

            self.emit(ExpectedClosingGenerics {
                span: (expected_span, self.file),
            });
        }

        Ok(Generics {
            span: token.to(self.prev().span),
            params: vec![],
        })
    }

    pub fn parse_generics_params(&mut self) -> PResult<'a, Vec<GenericParam>> {
        let mut params: Vec<GenericParam> = vec![];

        loop {
            if self.is_ident() {
                let ident = self.parse_ident()?;

                let default = if self.eat(TokenKind::Colon) {
                    let def = self.parse_type()?;
                    Some(def)
                } else {
                    None
                };

                params.push(GenericParam {
                    id: NodeId::max(),
                    ident,
                    kind: GenericKind::Type { default },
                })
            } else if self.eat_keyword(ptok!(Const)) {
                let ident = self.parse_ident()?;

                let ty = {
                    self.expect(TokenKind::Colon)?;
                    self.parse_type()?
                };
                
                let const_expr = if self.eat(TokenKind::Eq) {
                    let expr = self.parse_const_expr()?;
                    Some(expr)
                } else {
                    None
                };
                params.push(GenericParam {
                    id: NodeId::max(),
                    ident,
                    kind: GenericKind::NonType { ty, default: const_expr },
                })
            } else {
                break;
            }
            
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }
}
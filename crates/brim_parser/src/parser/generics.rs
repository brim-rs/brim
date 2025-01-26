use crate::{
    parser::{PResult, PToken, PTokenKind, Parser, errors::ExpectedClosingGenerics},
    ptok,
};
use brim_ast::{
    Const,
    item::{GenericKind, GenericParam, Generics},
    token::TokenKind,
};
use brim_ast::item::{GenericArg, GenericArgs};

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
            params,
        })
    }

    /// Parses the generic arguments provided as an argument. eg: `foo<T, U>`
    pub fn parse_argument_generics(&mut self) -> PResult<'a, GenericArgs> {
        let token = self.current().span;
        if !self.eat(TokenKind::Lt) {
            // no generics. return early
            return Ok(GenericArgs {
                span: self.prev().span.from_end(),
                params: vec![],
            });
        }

        let params = {
            let mut params = vec![];

            loop {
                let ty = self.parse_type()?;

                params.push(GenericArg {
                    id: self.new_id(),
                    ty,
                });

                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }

            params
        };

        if !self.eat(TokenKind::Gt) {
            let expected_span = self.prev().span.from_end();

            self.emit(ExpectedClosingGenerics {
                span: (expected_span, self.file),
            });
        }

        Ok(GenericArgs {
            span: token.to(self.prev().span),
            params
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
                    id: self.new_id(),
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
                    id: self.new_id(),
                    ident,
                    kind: GenericKind::NonType {
                        ty,
                        default: const_expr,
                    },
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

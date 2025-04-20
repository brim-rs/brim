use crate::{
    parser::{PResult, PToken, PTokenKind, Parser, errors::ExpectedClosingGenerics},
    ptok,
};
use brim_ast::{
    Const,
    item::{GenericArg, GenericArgs, GenericKind, GenericParam, Generics},
    token::TokenKind,
};
use brim_middle::ExperimentalFeatureNotEnabled;
use brim_span::span::Span;

impl Parser {
    pub fn parse_generics(&mut self) -> Generics {
        let token = self.current().span;
        if !self.eat(TokenKind::Lt) {
            // no generics. return early
            return Generics { chevrons: None, span: self.prev().span.from_end(), params: vec![] };
        }
        let ochevron = token;

        let params = self.parse_generics_params();

        if !self.eat(TokenKind::Gt) {
            let expected_span = self.prev().span.from_end();

            self.emit(ExpectedClosingGenerics { span: (expected_span, self.file) });
        }
        let cchevron = self.prev().span;

        let span = token.to(self.prev().span);
        if !self.experimental.generics {
            self.emit(ExperimentalFeatureNotEnabled {
                span: (span, self.file),
                feature: "generics".to_string(),
                note: "enable the `generics` feature in brim.toml to use generics".to_string(),
            });
        }

        Generics { span, params, chevrons: Some((ochevron, cchevron)) }
    }

    /// Parses the generic arguments provided as an argument. eg: `foo<T, U>`
    pub fn parse_argument_generics(&mut self) -> GenericArgs {
        let token = self.current().span;
        if !self.eat(TokenKind::Lt) {
            // no generics. return early
            return GenericArgs { span: self.prev().span.from_end(), params: vec![], braces: None };
        }

        let params = {
            let mut params = vec![];

            loop {
                let ty = self.parse_type();

                params.push(GenericArg { id: self.new_id(), ty });

                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }

            params
        };

        let closing = self.current().span;
        if !self.eat(TokenKind::Gt) {
            let expected_span = self.prev().span.from_end();

            self.emit(ExpectedClosingGenerics { span: (expected_span, self.file) });
        }

        GenericArgs { span: token.to(self.prev().span), params, braces: Some((token, closing)) }
    }

    pub fn parse_generics_params(&mut self) -> Vec<GenericParam> {
        let mut params: Vec<GenericParam> = vec![];

        loop {
            if self.is_ident() {
                let ident = self.parse_ident();

                let default = if self.eat(TokenKind::Colon) {
                    let span = self.prev().span;
                    let def = self.parse_type();
                    Some((span, def))
                } else {
                    None
                };

                params.push(GenericParam {
                    id: self.new_id(),
                    ident,
                    kind: GenericKind::Type { default: default.clone().map(|x| x.1) },
                })
            } else if self.eat_keyword(ptok!(Const)) {
                let ident = self.parse_ident();

                let ty = {
                    self.expect(TokenKind::Colon);
                    self.parse_type()
                };

                let const_expr = if self.eat(TokenKind::Eq) {
                    let expr = self.parse_expr();
                    Some(expr)
                } else {
                    None
                };
                params.push(GenericParam {
                    id: self.new_id(),
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

        params
    }
}

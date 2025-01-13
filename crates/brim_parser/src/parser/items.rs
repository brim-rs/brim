use crate::{
    debug_ident,
    parser::{
        PResult, PToken, PTokenKind, Parser,
        errors::{
            EmptyBody, ExpectedIdentifier, InvalidFunctionSignature, InvalidModifierOrder,
            MissingParamList, SelfOutsideMethod, UnnecessarySelf,
        },
    },
    ptok,
};
use anyhow::{Result, bail};
use brim_ast::{
    Const, Fn, NodeId, SelfSmall,
    item::{Block, FnDecl, FnReturnType, FnSignature, Generics, Ident, Item, ItemKind, Param},
    token::{Delimiter, Orientation, TokenKind},
    ty,
    ty::Const,
};
use brim_diagnostics::box_diag;
use brim_span::span::Span;

#[derive(Debug, Clone, Copy)]
pub enum FunctionContext {
    Item,
    Trait,
    Impl,
}

impl FunctionContext {
    pub fn allows_self(&self) -> bool {
        match self {
            FunctionContext::Item => false,
            _ => true,
        }
    }

    pub fn allows_empty_body(&self) -> bool {
        match self {
            FunctionContext::Trait => true,
            _ => false,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn parse_item(&mut self) -> PResult<'a, Option<Item>> {
        if self.token_cursor.is_eof() {
            return Ok(None);
        }

        let span = self.current().span;
        let vis = self.parse_visibility();

        let (ident, kind) = if self.is_function() {
            self.parse_fn(span, FunctionContext::Item)?
        } else {
            return Ok(None);
        };
        self.eat_semis();

        Ok(Some(Item {
            id: self.new_id(),
            span,
            vis,
            kind,
            ident,
        }))
    }

    /// Function can only contain `const` before the `fn` keyword eg: `pub const fn foo() {}`
    pub fn is_function(&self) -> bool {
        self.current().is_keyword(Fn)
            || (self.current().is_keyword(Const) && self.ahead(1).is_keyword(Fn))
    }

    pub fn parse_fn(
        &mut self,
        span: Span,
        fn_ctx: FunctionContext,
    ) -> PResult<'a, (Ident, ItemKind)> {
        let span = self.current().span;
        let (generics, sig) = self.parse_fn_signature(fn_ctx)?;

        let body = self.parse_fn_body(fn_ctx)?;

        Ok((
            sig.name,
            ItemKind::Fn(FnDecl {
                sig,
                generics,
                body,
            }),
        ))
    }

    pub fn parse_fn_body(&mut self, fn_ctx: FunctionContext) -> PResult<'a, Option<Block>> {
        if self.eat(TokenKind::Semicolon) {
            if !fn_ctx.allows_empty_body() {
                self.emit(EmptyBody {
                    span: (self.prev().span, self.file),
                });
            }

            return Ok(None);
        }

        self.expect_obrace()?;
        let block = self.parse_block(false)?;
        self.expect_cbrace()?;

        Ok(Some(block))
    }

    pub fn parse_fn_signature(
        &mut self,
        fn_ctx: FunctionContext,
    ) -> PResult<'a, (Generics, FnSignature)> {
        let span = self.current().span;
        let constant = self.parse_constant();

        if !self.eat_keyword(ptok!(Fn)) {
            self.emit(InvalidFunctionSignature {
                span: (span, self.file),
                message: "Expected `fn` keyword".to_string(),
            });
        }

        // We additionally check if `const` is present after `fn` keyword.
        if self.ahead(0).is_keyword(Const) {
            self.emit(InvalidModifierOrder {
                span: (span, self.file),
                message: "`const` keyword should be placed before `fn` keyword".to_string(),
            });
        }

        let ident = self.parse_ident()?;
        let generics = self.parse_generics()?;
        let params = self.parse_fn_params(fn_ctx)?;

        let ret_type = self.parse_return_type()?;

        Ok((generics, FnSignature {
            constant,
            span,
            name: ident,
            params,
            return_type: ret_type,
        }))
    }

    pub fn parse_return_type(&mut self) -> PResult<'a, FnReturnType> {
        if self.eat(TokenKind::Arrow) {
            let ty = self.parse_type()?;
            Ok(FnReturnType::Ty(ty))
        } else {
            Ok(FnReturnType::Default)
        }
    }

    pub fn parse_fn_params(&mut self, fn_ctx: FunctionContext) -> PResult<'a, Vec<Param>> {
        let mut params = vec![];
        if !self
            .current_token
            .is_delimiter(Delimiter::Paren, Orientation::Open)
        {
            self.emit(MissingParamList {
                span: (self.prev().span.from_end(), self.file),
            });

            return Ok(params);
        }
        self.expect_oparen()?;

        while !self.is_paren(Orientation::Close) {
            let span_start = self.current().span;
            let ident = self.parse_ident()?;

            if ident.name == SelfSmall {
                if !fn_ctx.allows_self() {
                    box_diag!(SelfOutsideMethod {
                        span: (ident.span, self.file),
                        note: "this parameter would be unnecessary. `self` is always accessible in the right context",
                    });
                } else {
                    box_diag!(UnnecessarySelf {
                        span: (ident.span, self.file),
                    });
                }
            }

            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;

            params.push(Param {
                id: self.new_id(),
                span: span_start.to(self.prev().span),
                ty,
            });

            if !self.eat(TokenKind::Comma) {
                break;
            }
        }

        self.expect_cparen()?;

        Ok(params)
    }

    pub fn parse_ident(&mut self) -> PResult<'a, Ident> {
        let ident = match self.current().as_ident() {
            Some(ident) if ident.is_reserved() => self.expected_identifier_err()?,
            None => return self.expected_identifier_err(),
            Some(ident) => ident,
        };

        self.advance();

        Ok(ident)
    }

    pub fn expected_identifier_err(&self) -> PResult<'a, Ident> {
        let span = self.current().span;

        box_diag!(ExpectedIdentifier {
            span: (span, self.file),
            message: format!("but found `{}`", self.current().kind),
        });
    }

    pub fn parse_constant(&mut self) -> ty::Const {
        if self.eat_keyword(ptok!(Const)) {
            self.advance();

            Const::Yes
        } else {
            Const::No
        }
    }
}

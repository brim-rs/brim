use anyhow::bail;
use crate::parser::{PResult, Parser};
use anyhow::Result;
use brim::{box_diag, Const, Fn};
use brim::compiler::CompilerContext;
use brim::item::{FnReturnType, FnSignature, Ident, Item, ItemKind};
use brim::span::Span;
use brim::symbols::GLOBAL_INTERNER;
use brim::ty::Const;
use crate::{debug_ident, ptok};
use crate::parser::{PToken, PTokenKind};
use crate::parser::errors::{ExpectedIdentifier, InvalidFunctionSignature, InvalidModifierOrder};

impl<'a> Parser<'a> {
    pub fn parse_item(&mut self) -> PResult<'a, Option<Item>> {
        if self.token_cursor.is_eof() {
            return Ok(None);
        }

        let span = self.current().span;
        let token = self.current();
        let visibility = self.parse_visibility();

        let kind = if self.is_function() {
            self.parse_fn(span)?
        } else {
            return Ok(None)
        };

        Ok(None)
    }

    /// Function can only contain `const` before the `fn` keyword eg: `pub const fn foo() {}`
    pub fn is_function(&self) -> bool {
        self.current().is_keyword(Fn) || (self.current().is_keyword(Const) && self.ahead(1).is_keyword(Fn))
    }

    pub fn parse_fn(&mut self, span: Span) -> PResult<'a, ItemKind> {
        let span = self.current().span;
        let signature = self.parse_fn_signature()?;
        let generics = self.parse_generics()?;

        println!("{:?}", signature);
        todo!()
    }

    pub fn parse_fn_signature(&mut self) -> PResult<'a, FnSignature> {
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

        Ok(FnSignature {
            constant,
            span,
            name: ident,
            params: vec![],
            return_type: FnReturnType::Default,
        })
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

    pub fn parse_constant(&mut self) -> brim::ty::Const {
        if self.eat_keyword(ptok!(Const)) {
            self.advance();

            Const::Yes
        } else {
            Const::No
        }
    }
}

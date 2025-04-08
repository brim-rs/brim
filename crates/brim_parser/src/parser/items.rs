use crate::{
    parser::{
        PResult, PToken, PTokenKind, Parser,
        errors::{
            EmptyBody, ExpectedIdentifier, InvalidExternBlockItem, InvalidFunctionSignature,
            InvalidModifierOrder, MissingFromKeyword, MissingParamList, SelfOutsideMethod,
            UnknownItem, UseStatementBraces,
        },
    },
    ptok,
};
use brim_ast::{
    Const, Enum, Extern, Fn, From, Mod, SelfSmall, Struct, Type, Use,
    item::{
        Block, Enum as AstEnum, EnumField, EnumVariant, ExternBlock, Field, FnDecl, FnReturnType,
        FnSignature, FunctionContext, Generics, Ident, ImportsKind, Item, ItemKind, ModuleDecl,
        Param, Struct, Use,
    },
    token::{BinOpToken, Delimiter, LitKind, Orientation, TokenKind},
};
use brim_diagnostics::box_diag;
use brim_span::span::Span;
use tracing::debug;

impl Parser {
    pub fn parse_item(&mut self) -> PResult<Option<Item>> {
        if self.token_cursor.is_eof() {
            return Ok(None);
        }

        let span = self.current().span;
        let vis = self.parse_visibility();

        let (ident, kind) = if self.is_function() {
            self.set_fn_ctx(FunctionContext::Item);
            self.parse_fn()?
        } else if self.current().is_keyword(Use) {
            self.parse_use(span)?
        } else if self.current().is_keyword(Struct) {
            self.parse_struct(span)?
        } else if self.current().is_keyword(Type) {
            self.parse_type_alias()?
        } else if self.current().is_keyword(brim_ast::Mod) {
            self.parse_mod_decl()?
        } else if self.current().is_keyword(Extern) {
            self.parse_extern()?
        } else if self.current().is_keyword(brim_ast::Enum) {
            self.parse_enum()?
        } else {
            box_diag!(UnknownItem {
                span: (span, self.file),
                found: self.current().kind,
            })
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

    pub fn parse_enum(&mut self) -> PResult<(Ident, ItemKind)> {
        let span = self.current().span;
        self.eat_keyword(ptok!(Enum));
        let ident = self.parse_ident()?;
        let generics = self.parse_generics()?;
        self.expect_obrace()?;

        let mut variants = vec![];
        let mut items = vec![];

        while !self.is_brace(Orientation::Close) {
            if self.is_function()
                || self.current().is_keyword(Use)
                || self.current().is_keyword(Type)
            {
                break;
            }

            let variant = self.parse_enum_variant()?;
            variants.push(variant);

            if !self.eat(TokenKind::Comma) {
                break;
            }
        }

        while !self.is_brace(Orientation::Close) {
            let item_span = self.current().span;
            let vis = self.parse_visibility();

            let (item_ident, kind) = if self.is_function() {
                self.set_fn_ctx(FunctionContext::Method);
                self.parse_fn()?
            } else if self.current().is_keyword(Use) {
                self.parse_use(item_span)?
            } else if self.current().is_keyword(Type) {
                self.parse_type_alias()?
            } else {
                break;
            };

            self.eat_semis();

            items.push(Item {
                id: self.new_id(),
                span: item_span,
                vis,
                kind,
                ident: item_ident,
            });
        }

        self.expect_cbrace()?;

        debug!(
            "Parsed enum: {:?} with {} variants and {} items",
            ident,
            variants.len(),
            items.len()
        );

        Ok((
            ident,
            ItemKind::Enum(AstEnum {
                span: span.to(self.prev().span),
                generics,
                variants,
                items,
                ident,
            }),
        ))
    }

    pub fn parse_enum_variant(&mut self) -> PResult<EnumVariant> {
        let ident = self.parse_ident()?;
        let mut fields = vec![];

        if self.is_paren(Orientation::Open) {
            self.expect_oparen()?;
            while !self.is_paren(Orientation::Close) {
                let ty = self.parse_type()?;
                fields.push(EnumField {
                    span: ty.span.to(self.current().span),
                    ty,
                });
                self.eat_possible(TokenKind::Comma);
            }
            self.expect_cparen()?;
        }

        Ok(EnumVariant {
            span: ident.span.to(self.current().span),
            ident,
            fields,
        })
    }

    pub fn parse_struct(&mut self, span: Span) -> PResult<(Ident, ItemKind)> {
        self.eat_keyword(ptok!(Struct));

        let ident = self.parse_ident()?;
        let generics = self.parse_generics()?;

        self.expect_obrace()?;

        let mut fields = vec![];
        let mut items = vec![];

        while !self.is_brace(Orientation::Close) {
            if self.is_function() || self.current().is_keyword(Use) {
                break;
            }

            let vis = self.parse_visibility();
            let ident = self.parse_ident()?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;

            fields.push(Field {
                id: self.new_id(),
                span: vis.span.to(self.prev().span),
                ident,
                ty,
                vis,
            });

            if !self.eat(TokenKind::Comma) {
                break;
            }
        }

        while !self.is_brace(Orientation::Close) {
            let item_span = self.current().span;
            let vis = self.parse_visibility();

            let (item_ident, kind) = if self.is_function() {
                self.set_fn_ctx(FunctionContext::Method);
                self.parse_fn()?
            } else if self.current().is_keyword(Use) {
                self.parse_use(item_span)?
            } else if self.current().is_keyword(Type) {
                self.parse_type_alias()?
            } else {
                break;
            };

            self.eat_semis();

            items.push(Item {
                id: self.new_id(),
                span: item_span,
                vis,
                kind,
                ident: item_ident,
            });
        }

        self.expect_cbrace()?;

        debug!(
            "Parsed struct: {:?} with {} fields and {} items",
            ident,
            fields.len(),
            items.len()
        );

        Ok((
            ident,
            ItemKind::Struct(Struct {
                ident,
                generics,
                span,
                fields,
                items,
            }),
        ))
    }

    pub fn parse_extern(&mut self) -> PResult<(Ident, ItemKind)> {
        let span = self.current().span;
        self.eat_keyword(ptok!(Extern));

        let abi = match self.current().kind {
            TokenKind::Literal(lit) => match lit.kind {
                LitKind::Str => {
                    self.advance();
                    Some(lit.symbol)
                }
                _ => None,
            },
            _ => None,
        };

        self.expect_obrace()?;

        let mut items = vec![];

        loop {
            self.fn_ctx = Some(FunctionContext::Extern);

            if self.is_brace(Orientation::Close) {
                break;
            }

            let item = self.parse_item()?;

            if let Some(item) = item {
                if !matches!(item.kind, ItemKind::Fn(_) | ItemKind::TypeAlias(_)) {
                    self.emit(InvalidExternBlockItem {
                        span: (item.span, self.file),
                        found: item.kind.to_string(),
                    });
                }

                items.push(item);
            }
        }
        self.fn_ctx = None;

        self.expect_cbrace()?;

        Ok((
            Ident::dummy(),
            ItemKind::External(ExternBlock {
                span: span.to(self.prev().span),
                abi,
                items,
            }),
        ))
    }

    pub fn parse_mod_decl(&mut self) -> PResult<(Ident, ItemKind)> {
        let mut idents = vec![];

        self.eat_keyword(ptok!(Mod));

        while self.is_ident() {
            let ident = self.parse_ident()?;
            idents.push(ident);

            if !self.eat(TokenKind::DoubleColon) {
                break;
            }
        }

        debug!("Parsed module declaration: {:?}", idents);
        Ok((
            Ident::dummy(),
            ItemKind::Module(ModuleDecl {
                span: idents[0].span.to(self.prev().span),
                idents,
            }),
        ))
    }

    pub fn parse_use(&mut self, span: Span) -> PResult<(Ident, ItemKind)> {
        self.eat_keyword(ptok!(Use));

        let kind = if self.current().kind == TokenKind::BinOp(BinOpToken::Star) {
            self.advance();
            ImportsKind::All
        } else if self.is_brace(Orientation::Open) {
            self.advance();

            let mut imports = vec![];

            while !self.is_brace(Orientation::Close) {
                let ident = self.parse_ident()?;
                imports.push(ident);

                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }

            self.expect_cbrace()?;
            ImportsKind::List(imports)
        } else if self.is_ident() {
            let ident = self.parse_ident()?;
            ImportsKind::Default(ident)
        } else {
            box_diag!(UseStatementBraces {
                span: (span.to(self.current().span), self.file),
            });
        };

        if !self.eat_keyword(ptok!(From)) {
            self.emit(MissingFromKeyword {
                span: (self.prev().span.from_end(), self.file),
            });
        }

        let path = self.expect_str_literal()?;

        debug!("Parsed use statement: {:?}", path);
        Ok((
            Ident::dummy(),
            ItemKind::Use(Use {
                span: span.to(self.current().span),
                imports: kind,
                path,
                resolved: None,
            }),
        ))
    }

    /// Function can only contain `const` before the `fn` keyword eg: `pub const fn foo() {}`
    pub fn is_function(&self) -> bool {
        self.current().is_keyword(Fn)
            || (self.current().is_keyword(Const) && self.ahead(1).is_keyword(Fn))
    }

    pub fn parse_fn(&mut self) -> PResult<(Ident, ItemKind)> {
        let (generics, sig) = self.parse_fn_signature()?;

        let body = self.parse_fn_body()?;

        debug!("=== Parsed function: {:?}", sig);

        let copy = self.fn_ctx().clone();
        self.fn_ctx = None;
        Ok((
            sig.name,
            ItemKind::Fn(FnDecl {
                sig,
                generics,
                body,
                context: copy,
            }),
        ))
    }

    pub fn parse_fn_body(&mut self) -> PResult<Option<Block>> {
        if self.eat(TokenKind::Semicolon) {
            if !self.fn_ctx().allows_empty_body() {
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

    pub fn parse_fn_signature(&mut self) -> PResult<(Generics, FnSignature)> {
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
        debug!("=== Starting to parse function: {}", ident);

        let generics = self.parse_generics()?;
        let params = self.parse_fn_params()?;

        let ret_type = self.parse_return_type()?;

        Ok((
            generics,
            FnSignature {
                constant,
                span: span.to(self.prev().span),
                name: ident,
                params,
                return_type: ret_type,
            },
        ))
    }

    pub fn parse_return_type(&mut self) -> PResult<FnReturnType> {
        if self
            .current()
            .is_delimiter(Delimiter::Brace, Orientation::Open)
            || self.current().kind == TokenKind::Semicolon
        {
            Ok(FnReturnType::Default)
        } else {
            let ty = self.parse_type()?;
            Ok(FnReturnType::Ty(ty))
        }
    }

    pub fn parse_fn_params(&mut self) -> PResult<Vec<Param>> {
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
                if !self.fn_ctx().allows_self() {
                    box_diag!(SelfOutsideMethod {
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
                name: ident,
            });

            if !self.eat(TokenKind::Comma) {
                break;
            }
        }

        self.expect_cparen()?;

        Ok(params)
    }

    pub fn parse_ident(&mut self) -> PResult<Ident> {
        let ident = match self.current().as_ident() {
            Some(ident) if ident.is_reserved() => self.expected_identifier_err()?,
            None => return self.expected_identifier_err(),
            Some(ident) => ident,
        };

        self.advance();

        Ok(ident)
    }

    pub fn parse_ident_without_err(&mut self) -> PResult<Option<Ident>> {
        let ident = match self.current().as_ident() {
            Some(ident) if ident.is_reserved() => return Ok(None),
            None => return Ok(None),
            Some(ident) => ident,
        };

        self.advance();

        Ok(Some(ident))
    }

    pub fn expected_identifier_err(&self) -> PResult<Ident> {
        let span = self.current().span;

        box_diag!(ExpectedIdentifier {
            span: (span, self.file),
            message: format!("but found `{}`", self.current().kind),
        });
    }

    pub fn parse_constant(&mut self) -> bool {
        if self.eat_keyword(ptok!(Const)) {
            self.advance();

            true
        } else {
            false
        }
    }
}

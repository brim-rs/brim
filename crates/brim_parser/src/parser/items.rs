use crate::{
    parser::{
        PResult, PToken, PTokenKind, Parser,
        errors::{
            EmptyBody, ExpectedIdentifier, InvalidExternBlockItem, InvalidFunctionSignature,
            InvalidModifierOrder, InvalidUseItems, MissingFromKeyword, MissingParamList,
            SelfOutsideMethod, UnknownItem,
        },
    },
    ptok,
};
use brim_ast::{
    Const, Enum, Extern, Fn, From, Mod, SelfSmall, Struct, Type, Use,
    item::{
        Attribute, Block, Enum as AstEnum, EnumField, EnumVariant, ExternBlock, Field, FnDecl,
        FnReturnType, FnSignature, FunctionContext, Generics, Ident, ImportsKind, Item, ItemKind,
        ModuleDecl, Param, Struct, Use,
    },
    token::{BinOpToken, Delimiter, LitKind, Orientation, TokenKind},
};
use brim_span::span::Span;
use tracing::debug;

impl Parser {
    pub fn parse_item(&mut self) -> Option<Item> {
        if self.token_cursor.is_eof() {
            return None;
        }
        let attrs = self.parse_attributes();

        let span = self.current().span;
        let vis = self.parse_visibility();

        let (ident, kind) = if self.is_function() {
            self.set_fn_ctx(FunctionContext::Item);
            self.parse_fn()
        } else if self.current().is_keyword(Use) {
            self.parse_use(span)
        } else if self.current().is_keyword(Struct) {
            self.parse_struct(span)
        } else if self.current().is_keyword(Type) {
            self.parse_type_alias()
        } else if self.current().is_keyword(Mod) {
            self.parse_mod_decl()
        } else if self.current().is_keyword(Extern) {
            self.parse_extern()
        } else if self.current().is_keyword(Enum) {
            self.parse_enum()
        } else {
            panic!("error");
            // box_diag!(UnknownItem { span: (span, self.file), found: self.current().kind })
        };
        let semis = self.eat_semis();

        Some(Item { id: self.new_id(), span, vis, kind, ident, attrs, semis })
    }

    pub fn eat_semis(&mut self) -> Vec<Span> {
        let mut semis = vec![];

        while self.current().is(TokenKind::Semicolon) {
            semis.push(self.current().span);
            self.advance();
        }
        semis
    }

    pub fn parse_attributes(&mut self) -> Vec<Attribute> {
        let mut attrs = vec![];

        while self.current().is(TokenKind::At) {
            let attr = self.parse_attribute();
            attrs.push(attr);
        }

        attrs
    }

    pub fn parse_enum(&mut self) -> (Ident, ItemKind) {
        let span = self.current().span;
        self.eat_keyword(ptok!(Enum));
        let keyword = self.prev().span;
        let ident = self.parse_ident();
        let generics = self.parse_generics();
        self.expect_obrace();

        let mut variants = vec![];
        let mut items = vec![];

        while !self.is_brace(Orientation::Close) {
            if self.is_function()
                || self.current().is_keyword(Use)
                || self.current().is_keyword(Type)
            {
                break;
            }

            let variant = self.parse_enum_variant();
            variants.push(variant);

            if !self.eat(TokenKind::Comma) {
                break;
            }
        }

        while !self.is_brace(Orientation::Close) {
            let item_span = self.current().span;
            let attrs = self.parse_attributes();
            let vis = self.parse_visibility();

            let (item_ident, kind) = if self.is_function() {
                self.set_fn_ctx(FunctionContext::Method);
                self.parse_fn()
            } else if self.current().is_keyword(Use) {
                self.parse_use(item_span)
            } else if self.current().is_keyword(Type) {
                self.parse_type_alias()
            } else {
                break;
            };

            let semis = self.eat_semis();

            items.push(Item {
                id: self.new_id(),
                span: item_span,
                vis,
                kind,
                ident: item_ident,
                attrs,
                semis,
            });
        }

        self.expect_cbrace();

        debug!(
            "Parsed enum: {:?} with {} variants and {} items",
            ident,
            variants.len(),
            items.len()
        );

        (
            ident,
            ItemKind::Enum(AstEnum {
                span: span.to(self.prev().span),
                generics,
                variants,
                items,
                ident,
            }),
        )
    }

    pub fn parse_attribute(&mut self) -> Attribute {
        let span = self.current().span;
        self.expect(TokenKind::At);
        let ident = self.parse_ident();
        let args = if self.is_paren(Orientation::Open) {
            self.expect_oparen();
            let mut args = vec![];
            while !self.is_paren(Orientation::Close) {
                let arg = self.parse_expr();
                args.push(arg);
                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }
            self.expect_cparen();
            Some(args)
        } else {
            None
        };

        Attribute {
            name: ident,
            args: args.unwrap_or(vec![]),
            span: span.to(self.prev().span),
            at_span: span,
        }
    }

    pub fn parse_enum_variant(&mut self) -> EnumVariant {
        let ident = self.parse_ident();
        let mut fields = vec![];

        if self.is_paren(Orientation::Open) {
            self.expect_oparen();
            while !self.is_paren(Orientation::Close) {
                let ty = self.parse_type();
                fields.push(EnumField { span: ty.span.to(self.current().span), ty });
                self.eat_possible(TokenKind::Comma);

                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }
            self.expect_cparen();
        }

        EnumVariant { span: ident.span.to(self.current().span), ident, fields, id: self.new_id() }
    }

    pub fn parse_struct(&mut self, span: Span) -> (Ident, ItemKind) {
        self.eat_keyword(ptok!(Struct));

        let ident = self.parse_ident();
        let generics = self.parse_generics();

        self.expect_obrace();

        let mut fields = vec![];
        let mut items = vec![];

        while !self.is_brace(Orientation::Close) {
            if self.is_function() || self.current().is_keyword(Use) {
                break;
            }

            let vis = self.parse_visibility();
            let ident = self.parse_ident();
            self.expect(TokenKind::Colon);
            let colon = self.prev().span;
            let ty = self.parse_type();

            fields.push(Field {
                id: self.new_id(),
                span: vis.span.to(self.prev().span),
                ident,
                ty,
                vis,
                colon,
            });

            if !self.eat(TokenKind::Comma) {
                break;
            }
        }

        while !self.is_brace(Orientation::Close) {
            let item_span = self.current().span;
            let attrs = self.parse_attributes();
            let vis = self.parse_visibility();

            let (item_ident, kind) = if self.is_function() {
                self.set_fn_ctx(FunctionContext::Method);
                self.parse_fn()
            } else if self.current().is_keyword(Use) {
                self.parse_use(item_span)
            } else if self.current().is_keyword(Type) {
                self.parse_type_alias()
            } else {
                break;
            };

            let semis = self.eat_semis();

            items.push(Item {
                id: self.new_id(),
                span: item_span,
                vis,
                kind,
                ident: item_ident,
                attrs,
                semis,
            });
        }

        self.expect_cbrace();

        debug!("Parsed struct: {:?} with {} fields and {} items", ident, fields.len(), items.len());

        (ident, ItemKind::Struct(Struct { ident, generics, span, fields, items }))
    }

    pub fn parse_extern(&mut self) -> (Ident, ItemKind) {
        let span = self.current().span;
        self.eat_keyword(ptok!(Extern));

        let abi_span = self.current().span;
        let abi = match self.current().kind {
            TokenKind::Literal(lit) => match lit.kind {
                LitKind::Str => {
                    self.advance();
                    Some((lit.symbol, abi_span.to(self.prev().span)))
                }
                _ => None,
            },
            _ => None,
        };

        self.expect_obrace();

        let mut items = vec![];

        loop {
            self.fn_ctx = Some(FunctionContext::Extern);

            if self.is_brace(Orientation::Close) {
                break;
            }

            let item = self.parse_item();

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

        self.expect_cbrace();

        (
            Ident::dummy(),
            ItemKind::External(ExternBlock { span: span.to(self.prev().span), abi, items }),
        )
    }

    pub fn parse_mod_decl(&mut self) -> (Ident, ItemKind) {
        let mut idents = vec![];

        self.eat_keyword(ptok!(Mod));
        while self.is_ident() {
            let ident = self.parse_ident();
            idents.push(ident);

            if !self.eat(TokenKind::DoubleColon) {
                break;
            }
        }

        debug!("Parsed module declaration: {:?}", idents);
        (
            Ident::dummy(),
            ItemKind::Module(ModuleDecl { span: idents[0].span.to(self.prev().span), idents }),
        )
    }

    pub fn parse_use(&mut self, span: Span) -> (Ident, ItemKind) {
        let use_span = self.current().span;
        self.eat_keyword(ptok!(Use));

        let kind = if self.current().kind == TokenKind::BinOp(BinOpToken::Star) {
            self.advance();
            ImportsKind::All
        } else if self.is_brace(Orientation::Open) {
            self.advance();

            let mut imports = vec![];

            while !self.is_brace(Orientation::Close) {
                let ident = self.parse_ident();
                imports.push(ident);

                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }

            self.expect_cbrace();
            ImportsKind::List(imports)
        } else if self.is_ident() {
            let ident = self.parse_ident();
            ImportsKind::Default(ident)
        } else {
            self.dcx.emit_impl(InvalidUseItems { span: (span.to(self.current().span), self.file) });

            ImportsKind::Default(Ident::dummy())
        };

        let from_span = self.current().span;
        if !self.eat_keyword(ptok!(From)) {
            self.emit(MissingFromKeyword { span: (self.prev().span.from_end(), self.file) });
        }

        let path_span = self.current().span;
        let path = self.expect_str_literal();

        debug!("Parsed use statement: {:?}", path);
        (
            Ident::dummy(),
            ItemKind::Use(Use {
                use_span,
                span: span.to(self.current().span),
                imports: kind,
                path,
                resolved: None,
                path_span,
                from_span,
            }),
        )
    }

    /// Function can only contain `const` before the `fn` keyword eg: `pub const fn foo() {}`
    pub fn is_function(&self) -> bool {
        self.current().is_keyword(Fn)
            || (self.current().is_keyword(Const) && self.ahead(1).is_keyword(Fn))
    }

    pub fn parse_fn(&mut self) -> (Ident, ItemKind) {
        let (generics, sig) = self.parse_fn_signature();

        let body = self.parse_fn_body();

        debug!("=== Parsed function: {:?}", sig);

        let copy = self.fn_ctx();
        self.fn_ctx = None;
        (sig.name, ItemKind::Fn(FnDecl { sig, generics, body, context: copy }))
    }

    pub fn parse_fn_body(&mut self) -> Option<Block> {
        if self.eat(TokenKind::Semicolon) {
            if !self.fn_ctx().allows_empty_body() {
                self.emit(EmptyBody { span: (self.prev().span, self.file) });
            }

            return None;
        }

        let block = self.parse_block(true);

        Some(block)
    }

    pub fn parse_fn_signature(&mut self) -> (Generics, FnSignature) {
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

        let ident = self.parse_ident();
        debug!("=== Starting to parse function: {}", ident);

        let generics = self.parse_generics();
        let params = self.parse_fn_params();

        let ret_type = self.parse_return_type();

        (generics, FnSignature {
            constant,
            span: span.to(self.prev().span),
            name: ident,
            params,
            return_type: ret_type,
        })
    }

    pub fn parse_return_type(&mut self) -> FnReturnType {
        if self.current().is_delimiter(Delimiter::Brace, Orientation::Open)
            || self.current().kind == TokenKind::Semicolon
        {
            FnReturnType::Default
        } else {
            let ty = self.parse_type();
            FnReturnType::Ty(ty)
        }
    }

    pub fn parse_fn_params(&mut self) -> Vec<Param> {
        let mut params = vec![];
        if !self.current_token.is_delimiter(Delimiter::Paren, Orientation::Open) {
            self.emit(MissingParamList { span: (self.prev().span.from_end(), self.file) });

            return params;
        }
        self.expect_oparen();

        while !self.is_paren(Orientation::Close) {
            let span_start = self.current().span;
            let ident = self.parse_ident();

            if ident.name == SelfSmall && !self.fn_ctx().allows_self() {
                self.dcx.emit_impl(SelfOutsideMethod { span: (ident.span, self.file) });
            }

            let colon = self.current().span;
            self.expect(TokenKind::Colon);
            let ty = self.parse_type();
            params.push(Param {
                id: self.new_id(),
                span: span_start.to(self.prev().span),
                ty,
                name: ident,
                colon,
            });

            if !self.eat(TokenKind::Comma) {
                break;
            }
        }

        self.expect_cparen();

        params
    }

    pub fn parse_ident(&mut self) -> Ident {
        let ident = match self.current().as_ident() {
            Some(ident) if ident.is_reserved() => {
                self.expected_identifier_err();
                Ident::dummy()
            }
            None => {
                self.expected_identifier_err();
                Ident::dummy()
            }
            Some(ident) => ident,
        };

        self.advance();

        ident
    }

    pub fn parse_ident_without_err(&mut self) -> Option<Ident> {
        let ident = match self.current().as_ident() {
            Some(ident) if ident.is_reserved() => return None,
            None => return None,
            Some(ident) => ident,
        };

        self.advance();

        Some(ident)
    }

    pub fn expected_identifier_err(&mut self) {
        let span = self.current().span;

        self.dcx.emit_impl(ExpectedIdentifier {
            span: (span, self.file),
            message: format!("but found `{}`", self.current().kind),
        });
    }

    pub fn parse_constant(&mut self) -> Option<Span> {
        let span = self.current().span;
        if self.eat_keyword(ptok!(Const)) {
            self.advance();

            Some(span)
        } else {
            None
        }
    }
}

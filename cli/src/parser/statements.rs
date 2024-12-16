use anyhow::{bail, Result};
use colored::Colorize;
use indexmap::IndexMap;
use crate::ast::statements::{Block, ElseBlock, FnParam, Function, StructField, TypeAnnotation};
use crate::ast::StmtId;
use crate::ast::types::TypeKind;
use crate::error::{expected_token, parser_error};
use crate::lexer::tokens::{Token, TokenKind};
use crate::parser::{ParseContext, Parser};

impl Parser {
    pub fn parse_stmt(&mut self) -> Result<Option<StmtId>> {
        let token = self.peek();

        let stmt = match token.kind {
            TokenKind::Pub => {
                if self.peek_next().kind == TokenKind::Fn {
                    Some(self.parse_fn()?)
                } else if self.peek_next().kind == TokenKind::Struct {
                    Some(self.parse_struct()?)
                } else if self.peek_next().kind == TokenKind::Trait {
                    Some(self.parse_trait()?)
                } else if self.peek_next().kind == TokenKind::Const {
                    Some(self.parse_const()?)
                } else {
                    // TODO: return error
                    None
                }
            }
            TokenKind::Fn => Some(self.parse_fn()?),
            TokenKind::Struct => Some(self.parse_struct()?),
            TokenKind::Trait => Some(self.parse_trait()?),
            TokenKind::Const => Some(self.parse_const()?),
            TokenKind::Impl => {
                let impl_keyword = self.consume();
                if self.peek().kind == TokenKind::Identifier {
                    let ident = self.consume();

                    if self.peek().kind == TokenKind::For {
                        Some(self.parse_trait_impl(impl_keyword, ident)?)
                    } else if self.peek().kind == TokenKind::LeftBrace {
                        Some(self.parse_impl(impl_keyword, ident)?)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            TokenKind::Use => Some(self.parse_use()?),
            TokenKind::If => Some(self.parse_if()?),
            TokenKind::Let => Some(self.parse_let()?),
            TokenKind::Try => Some(self.parse_try()?),
            TokenKind::Break => {
                self.consume();
                self.possible_check(TokenKind::Semicolon);
                Some(self.ast.new_break_stmt(token))
            }
            TokenKind::Continue => {
                self.consume();
                self.possible_check(TokenKind::Semicolon);
                Some(self.ast.new_continue_stmt(token))
            }
            TokenKind::Loop => {
                self.consume();
                let block = self.parse_block()?;
                Some(self.ast.new_loop(block, token))
            }
            TokenKind::While => self.parse_while()?,
            TokenKind::LeftBrace => {
                self.consume();
                let block = self.parse_block()?;
                self.expect_punct(TokenKind::RightBrace)?;
                Some(block)
            }
            TokenKind::Return => self.parse_return()?,
            TokenKind::Semicolon | TokenKind::Comment => {
                self.consume();
                None
            }
            _ => Some(self.expression_stmt()?),
        };

        Ok(stmt)
    }

    pub fn parse_impl(&mut self, impl_keyword: Token, ident: Token) -> Result<StmtId> {
        self.expect_punct(TokenKind::LeftBrace)?;

        let mut methods: Vec<StmtId> = vec![];

        while self.peek().kind != TokenKind::RightBrace && !self.is_eof() {
            let func = self.parse_fn()?;

            methods.push(func);
        }

        self.expect_punct(TokenKind::RightBrace)?;

        Ok(self.ast.new_struct_impl(impl_keyword, ident, methods))
    }

    pub fn parse_trait_impl(&mut self, impl_keyword: Token, ident: Token) -> Result<StmtId> {
        let for_token = self.expect(TokenKind::For)?;

        let trait_name = self.expect(TokenKind::Identifier)?;

        self.expect_punct(TokenKind::LeftBrace)?;

        let mut methods: Vec<StmtId> = vec![];

        while self.peek().kind != TokenKind::RightBrace && !self.is_eof() {
            let func = self.parse_fn()?;

            methods.push(func);
        }

        self.expect_punct(TokenKind::RightBrace)?;

        Ok(self.ast.new_trait_impl(
            impl_keyword,
            ident,
            for_token,
            trait_name,
            methods,
        ))
    }

    pub fn parse_pub(&mut self, _: TokenKind) -> Result<(Token, bool)> {
        let mut public = false;
        let token = if self.peek().kind == TokenKind::Pub {
            self.consume();
            public = true;
            self.consume()
        } else {
            self.consume()
        };

        Ok((token, public))
    }

    pub fn parse_trait(&mut self) -> Result<StmtId> {
        let (trait_token, public) = self.parse_pub(TokenKind::Trait)?;

        let name = self.expect(TokenKind::Identifier)?;

        self.expect_punct(TokenKind::LeftBrace)?;

        let mut methods: Vec<StmtId> = vec![];

        while self.peek().kind != TokenKind::RightBrace && !self.is_eof() {
            let func = self.parse_fn()?;
            methods.push(func);
        }

        self.expect_punct(TokenKind::RightBrace)?;

        Ok(self.ast.new_trait_def(trait_token, name, methods, public))
    }

    pub fn parse_const(&mut self) -> Result<StmtId> {
        let (_, public) = self.parse_pub(TokenKind::Const)?;

        let name = self.expect(TokenKind::Identifier)?;

        self.expect(TokenKind::Equals)?;

        let expr = self.parse_expr()?;

        Ok(self.ast.new_const(expr, name, public))
    }

    pub fn parse_struct(&mut self) -> Result<StmtId> {
        let (struct_token, public) = self.parse_pub(TokenKind::Struct)?;
        let name = self.expect(TokenKind::Identifier)?;

        self.expect_punct(TokenKind::LeftBrace)?;

        if self.peek().kind != TokenKind::RightBrace && self.peek().kind != TokenKind::Identifier {
            return Err(expected_token(
                "field declaration".to_string(),
                vec![format!(
                    "Expected field declaration or '}}' after struct declaration, found '{}'",
                    self.peek().literal()
                )],
                vec![
                    (self.previous().span.clone(), None)
                ],
            )
                .into());
        }

        let mut fields = IndexMap::new();
        while self.peek().kind != TokenKind::RightBrace && !self.is_eof() {
            let ident = self.expect(TokenKind::Identifier)?;
            let type_annotation = self.parse_type_annotation(true)?;

            fields.insert(
                ident.literal(),
                StructField {
                    ident,
                    type_annotation,
                },
            );

            if self.peek().kind != TokenKind::RightBrace && self.peek().kind != TokenKind::Comma {
                return Err(expected_token(
                    "comma or '}}'".to_string(),
                    vec![format!(
                        "Every field except the last one must be followed by a comma, found '{}'",
                        self.peek().literal()
                    )],
                    vec![
                        (self.previous().span.clone(), None)
                    ],
                )
                    .into());
            } else {
                self.possible_check(TokenKind::Comma);
            }
        }

        self.expect_punct(TokenKind::RightBrace)?;

        Ok(self.ast.new_struct(struct_token, name, fields, public))
    }

    pub fn parse_while(&mut self) -> Result<Option<StmtId>> {
        let while_token = self.consume();

        self.push_context(ParseContext::WhileCondition);
        let condition = self.parse_expr()?;
        self.pop_context();

        self.expect_punct(TokenKind::LeftBrace)?;
        let block = self.parse_block()?;
        self.expect_punct(TokenKind::RightBrace)?;

        Ok(Some(self.ast.new_while(while_token, condition, block)))
    }

    pub fn parse_try(&mut self) -> Result<StmtId> {
        let try_token = self.consume();
        let try_expression = self.parse_expr()?;

        Ok(self.ast.new_try(
            try_token,
            try_expression,
        ))
    }

    pub fn parse_return(&mut self) -> Result<Option<StmtId>> {
        let return_token = self.consume();
        let value = if self.peek().kind != TokenKind::Semicolon {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.possible_check(TokenKind::Semicolon);

        Ok(Some(self.ast.new_return(return_token, value)))
    }

    pub fn parse_let(&mut self) -> Result<StmtId> {
        self.expect(TokenKind::Let)?;
        let ident = self.expect(TokenKind::Identifier)?;
        let type_annotation = self.parse_optional_type_annotation()?;
        self.expect(TokenKind::Equals)?;
        let value = self.parse_expr()?;
        Ok(self.ast.new_let(ident, value, type_annotation))
    }

    pub fn parse_if(&mut self) -> Result<StmtId> {
        let if_token = self.consume();

        self.push_context(ParseContext::IfCondition);

        let condition = self.parse_expr()?;

        self.pop_context();

        self.expect_punct(TokenKind::LeftBrace)?;
        let body = self.parse_block()?;
        self.expect_punct(TokenKind::RightBrace)?;

        let mut elseif_blocks = vec![];
        let mut else_block: Option<ElseBlock> = None;

        while self.peek().kind == TokenKind::Else {
            self.consume();

            if self.peek().kind == TokenKind::If {
                self.consume();
                self.possible_check(TokenKind::LeftParen);

                self.push_context(ParseContext::IfCondition);
                let elseif_condition = self.parse_expr()?;
                self.pop_context();

                self.possible_check(TokenKind::RightParen);

                self.expect_punct(TokenKind::LeftBrace)?;
                let elseif_body = self.parse_block()?;
                self.expect_punct(TokenKind::RightBrace)?;

                elseif_blocks.push(ElseBlock {
                    condition: elseif_condition,
                    block: elseif_body,
                    else_if: true,
                });
            } else {
                self.expect_punct(TokenKind::LeftBrace)?;
                let else_body = self.parse_block()?;
                self.expect_punct(TokenKind::RightBrace)?;

                else_block = Some(ElseBlock {
                    condition: condition.clone(),
                    block: else_body,
                    else_if: false,
                });
            }
        }

        Ok(self.ast.new_if(
            if_token,
            condition.into(),
            body,
            elseif_blocks.into(),
            else_block,
        ))
    }

    pub fn parse_use(&mut self) -> Result<StmtId> {
        let use_token = self.consume();

        let mut items = vec![];

        self.expect_punct(TokenKind::LeftBrace)?;

        while self.peek().kind != TokenKind::RightBrace && !self.is_eof() {
            let item = self.expect(TokenKind::Identifier)?;

            if self.peek().kind != TokenKind::RightBrace {
                self.expect(TokenKind::Comma)?;
            }

            items.push(item);
        }

        self.expect_punct(TokenKind::RightBrace)?;

        self.expect(TokenKind::From)?;

        let from = if self.peek().is_string() {
            self.consume()
        } else {
            bail!(expected_token(
                "string".to_string(),
                vec!["Expected string that is valid module or file".to_string()],
                vec![(self.peek().span.clone(), None)],
            ))
        };

        Ok(self.ast.new_use(use_token, from, items))
    }

    pub fn is_nullable(&mut self) -> bool {
        if self.peek().kind == TokenKind::QuestionMark {
            self.consume();
            true
        } else {
            false
        }
    }

    fn parse_type(&mut self) -> Result<(Token, Vec<TypeAnnotation>, bool)> {
        let type_name = self.expect(TokenKind::Identifier)?;

        let generics = if self.peek().kind == TokenKind::LessThan {
            self.consume();
            let mut generics = vec![];
            while self.peek().kind != TokenKind::GreaterThan {
                let type_annotation = self.parse_type_annotation(false)?;
                generics.push(type_annotation);
                if self.peek().kind != TokenKind::GreaterThan {
                    self.expect(TokenKind::Comma)?;
                }
            }
            self.expect(TokenKind::GreaterThan)?;
            generics
        } else {
            vec![]
        };
        
        let is_array = if self.peek().kind == TokenKind::LeftBracket {
            self.consume();
            self.expect_punct(TokenKind::RightBracket)?;
           
            true
        } else {
            false
        };

        Ok((type_name, generics, is_array))
    }

    pub fn parse_type_annotation(&mut self, expect_colon: bool) -> Result<TypeAnnotation> {
        let colon = if expect_colon {
            if self.peek().kind != TokenKind::Colon {
                return Err(expected_token(
                    "colon and type name".to_string(),
                    vec![format!(
                        "Expected {} followed by a type name, found {}",
                        ":".cyan(),
                        self.peek().literal().cyan()
                    )],
                    vec![(self.peek().span.clone(), None)],
                )
                    .into());
            }

            Some(self.expect(TokenKind::Colon)?)
        } else {
            None
        };

        let (token, generics, is_array) = self.parse_type()?;

        let (can_be_error, error_type) = self.parse_error_type()?;

        Ok(TypeAnnotation {
            token_name: Some(token.clone()),
            kind: TypeKind::from_str(&token.literal(), is_array),
            is_nullable: self.is_nullable(),
            separator: colon,
            generics,
            can_be_error,
            error_type,
        })
    }

    pub fn parse_error_type(&mut self) -> Result<(bool, Option<Token>)> {
        if self.peek().kind == TokenKind::Bang {
            self.consume();
            if self.peek().kind == TokenKind::Identifier {
                Ok((true, Some(self.consume())))
            } else {
                Ok((true, None))
            }
        } else {
            Ok((false, None))
        }
    }

    pub fn parse_return_type(&mut self) -> Result<Option<TypeAnnotation>> {
        if self.peek().kind != TokenKind::Arrow && self.peek().kind != TokenKind::LeftBrace {
            return Err(expected_token(
                "arrow or left brace".to_string(),
                vec![format!(
                    "Expected '->' or '{{' after function parameters, found '{}'",
                    self.peek().literal()
                )],
                vec![(self.peek().span.clone(), None)],
            )
                .into());
        } else if self.peek().kind == TokenKind::Bang {
            self.consume();
            let err_type = if self.peek().kind == TokenKind::Identifier {
                Some(self.consume())
            } else {
                None
            };

            return Ok(
                Some(TypeAnnotation {
                    token_name: None,
                    kind: TypeKind::Void,
                    is_nullable: false,
                    separator: None,
                    generics: vec![],
                    can_be_error: true,
                    error_type: err_type,
                })
            );
        } else if self.peek().kind == TokenKind::LeftBrace {
            return Ok(None);
        }

        let arrow = self.consume(); // consume the arrow
        let (token, generics, is_array) = self.parse_type()?;

        let (can_be_error, error_type) = self.parse_error_type()?;

        Ok(Some(TypeAnnotation {
            token_name: Some(token.clone()),
            kind: TypeKind::from_str(&token.literal(), is_array),
            is_nullable: self.is_nullable(),
            separator: Some(arrow),
            generics,
            can_be_error,
            error_type,
        }))
    }

    pub fn parse_block(&mut self) -> Result<StmtId> {
        let mut stmts = vec![];

        while self.peek().kind != TokenKind::RightBrace && !self.is_eof() {
            let stmt = self.parse_stmt()?;

            if let Some(stmt) = stmt {
                stmts.push(stmt);
            }
        }

        Ok(self.ast.new_block(stmts))
    }

    pub fn parse_fn(&mut self) -> Result<StmtId> {
        self.possible_check(TokenKind::Comment);

        let (fn_token, public) = self.parse_pub(TokenKind::Fn)?;

        let name = self.expect(TokenKind::Identifier)?;

        self.expect(TokenKind::LeftParen)?;
        let mut params = vec![];

        let mut is_static = true;

        if self.peek().kind != TokenKind::RightParen {
            while self.peek().kind != TokenKind::RightParen && !self.is_eof() {
                self.possible_check(TokenKind::Comma);

                let param = self.consume();

                if param.literal() == "self" {
                    if !is_static {
                        bail!(parser_error(
                                "Self parameter must be first parameter in method".to_string(),
                                vec![(param.span.clone(), None)],
                                vec![],
                                None
                            ))
                    }

                    is_static = false;
                }

                let type_annotation = if param.literal() == "self" {
                    TypeAnnotation {
                        token_name: None,
                        kind: TypeKind::Custom("Self".to_string()),
                        is_nullable: false,
                        separator: None,
                        generics: vec![],
                        can_be_error: false,
                        error_type: None,
                    }
                } else {
                    self.parse_type_annotation(true)?
                };

                params.push(FnParam {
                    type_annotation,
                    ident: param,
                });
            }
        }

        if !is_static && params[0].ident.literal() != "self" {
            bail!(parser_error(
                "First parameter in method must be 'self'".to_string(),
                vec![(self.peek().span.clone(), None)],
                vec![],
                None
            ))
        }

        self.expect(TokenKind::RightParen)?;

        let return_type = self.parse_return_type()?;

        let body: Option<StmtId> = if self.peek().kind != TokenKind::LeftBrace {
            self.possible_check(TokenKind::Semicolon);
            None
        } else {
            self.expect_punct(TokenKind::LeftBrace)?;
            let block = self.parse_block()?;
            self.expect_punct(TokenKind::RightBrace)?;
            Some(block)
        };

        Ok(self.ast.new_fn(
            fn_token,
            name,
            params,
            body,
            public,
            return_type,
            is_static,
        ))
    }
}

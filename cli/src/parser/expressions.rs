use indexmap::IndexMap;
use crate::ast::expressions::{AssignOperator, BinOpAssociativity, BinOpKind, BinOperator, Expr, UnOpKind, UnOperator};
use crate::lexer::tokens::{Token, TokenKind};
use crate::parser::{ParseContext, Parser};
use anyhow::{bail, Result};
use crate::ast::{ExprId, StmtId};
use crate::ast::statements::TypeAnnotation;
use crate::error::{expected_token, parser_error};

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Result<ExprId> {
        self.parse_assignment()
    }

    pub fn expression_stmt(&mut self) -> Result<StmtId> {
        let expr = self.parse_expr()?;

        self.possible_check(TokenKind::Semicolon);

        Ok(self.ast.new_expr_stmt(expr))
    }

    pub fn parse_binary_expression(&mut self) -> Result<ExprId> {
        let left = self.parse_unary_expression()?;
        self.parse_binary_expression_recurse(left, 0)
    }

    fn parse_binary_operator(&mut self) -> Option<BinOperator> {
        let token = self.peek();
        let kind = match token.kind {
            TokenKind::Plus => Some(BinOpKind::Plus),
            TokenKind::Minus => Some(BinOpKind::Minus),
            TokenKind::Asterisk => Some(BinOpKind::Multiply),
            TokenKind::Slash => Some(BinOpKind::Divide),
            TokenKind::Ampersand => Some(BinOpKind::BitwiseAnd),
            TokenKind::Pipe => Some(BinOpKind::BitwiseOr),
            TokenKind::Caret => Some(BinOpKind::BitwiseXor),
            TokenKind::DoubleAsterisk => Some(BinOpKind::Power),
            TokenKind::EqualsEquals => Some(BinOpKind::Equals),
            TokenKind::BangEquals => Some(BinOpKind::BangEquals),
            TokenKind::LessThan => Some(BinOpKind::LessThan),
            TokenKind::LessThanEquals => Some(BinOpKind::LessThanOrEqual),
            TokenKind::GreaterThan => Some(BinOpKind::GreaterThan),
            TokenKind::GreaterThanEquals => Some(BinOpKind::GreaterThanOrEqual),
            TokenKind::Percent => Some(BinOpKind::Modulo),
            TokenKind::And => Some(BinOpKind::And),
            TokenKind::Or => Some(BinOpKind::Or),
            TokenKind::Increment => Some(BinOpKind::Increment),
            TokenKind::Decrement => Some(BinOpKind::Decrement),
            TokenKind::DoubleGreaterThan => Some(BinOpKind::ShiftRight),
            TokenKind::DoubleLessThan => Some(BinOpKind::ShiftLeft),
            _ => None,
        };
        kind.map(|kind| BinOperator::new(kind, token.clone()))
    }

    pub fn parse_binary_expression_recurse(
        &mut self,
        mut left: ExprId,
        precedence: u8,
    ) -> Result<ExprId> {
        while let Some(operator) = self.parse_binary_operator() {
            let operator_precedence = operator.precedence();
            if operator_precedence < precedence {
                break;
            }

            self.consume();

            let mut right = self.parse_unary_expression()?;

            while let Some(next_operator) = self.parse_binary_operator() {
                let next_precedence = next_operator.precedence();

                if next_precedence > operator_precedence
                    || (next_precedence == operator_precedence
                    && next_operator.associativity() == BinOpAssociativity::Right)
                {
                    right = self.parse_binary_expression_recurse(right, next_precedence)?;
                } else {
                    break;
                }
            }

            left = self.ast.new_binary(left, operator, right);
        }

        Ok(left)
    }

    pub fn parse_unary_operator(&mut self) -> Option<UnOperator> {
        let token = self.peek();
        let kind = match token.kind {
            TokenKind::Minus => Some(UnOpKind::Minus),
            TokenKind::Tilde => Some(UnOpKind::BitwiseNot),
            TokenKind::Bang => Some(UnOpKind::LogicalNot),
            _ => None,
        };
        kind.map(|kind| UnOperator::new(kind, token.clone()))
    }

    pub fn parse_unary_expression(&mut self) -> Result<ExprId> {
        if let Some(operator) = self.parse_unary_operator() {
            let token = self.consume();
            let operand = self.parse_unary_expression()?;
            return Ok(self.ast.new_unary(operator, operand, token));
        }
        self.parse_access_expression()
    }

    pub fn parse_access_expression(&mut self) -> Result<ExprId> {
        let mut expr = self.parse_primary_expression()?;
        let mut token = self.peek();

        loop {
            if token.kind == TokenKind::Dot {
                self.consume();

                let field_token = self.consume();
                let mut field_expr = self.ast.new_variable(field_token.clone(), field_token.literal());

                if self.peek().kind == TokenKind::LeftParen {
                    field_expr = self.parse_call_expr(field_token)?;
                }

                expr = self.ast.new_field_access(expr, field_expr, token);
            } else if token.kind == TokenKind::LeftBracket {
                self.consume();
                let index = self.parse_expr()?;
                self.expect(TokenKind::RightBracket)?;
                expr = self.ast.new_index_access(expr, index, token);
            } else if token.kind == TokenKind::DoubleColon {
                let colons = self.consume();
                let field = self.parse_expr()?;

                expr = self.ast.new_static_method_access(expr, field, colons);
            } else {
                break;
            }
            token = self.peek();
        }

        Ok(expr)
    }

    pub fn parse_struct_constructor(&mut self, identifier: Token) -> Result<ExprId> {
        self.expect_punct(TokenKind::LeftBrace)?;

        let mut fields = IndexMap::new();

        while self.peek().kind != TokenKind::RightBrace && !self.is_eof() {
            let field_name = self.consume();
            self.expect(TokenKind::Colon)?;
            let field_value = self.parse_expr()?;

            fields.insert(field_name.literal(), field_value);

            self.possible_check(TokenKind::Comma);
        }

        self.expect_punct(TokenKind::RightBrace)?;

        Ok(self.ast.new_struct_constructor(
            identifier.literal(),
            fields,
            identifier,
        ))
    }

    pub fn parse_primary_expression(&mut self) -> Result<ExprId> {
        let token = self.consume();

        match &token.kind {
            TokenKind::Integer(int) => Ok(self.ast.new_integer(*int, token.clone())),
            TokenKind::Float(float) => Ok(self.ast.new_float(*float, token.clone())),
            TokenKind::Null => Ok(self.ast.new_null(token)),
            TokenKind::True | TokenKind::False => {
                Ok(self.ast.new_boolean(token.as_bool().unwrap(), token.clone()))
            }
            TokenKind::LeftBracket => self.parse_array(),
            TokenKind::LeftBrace => {
                let mut fields: IndexMap<String, ExprId> = IndexMap::new();

                while self.peek().kind != TokenKind::RightBrace && !self.is_eof() {
                    let field_name = {
                        if matches!(self.peek().kind, TokenKind::String(_)) {
                            self.consume()
                        } else {
                            bail!(expected_token(
                                "string literal".to_string(),
                                vec!["Field names in objects must be string literals.".to_string()],
                                vec![
                                    (self.peek().span.clone(), "Expected a string literal".to_string().into())
                                ],
                            ))
                        }
                    };

                    self.expect(TokenKind::Colon)?;
                    let field_value = self.parse_expr()?;

                    fields.insert(
                        field_name
                            .literal()
                            .strip_prefix("\"")
                            .unwrap()
                            .strip_suffix("\"")
                            .unwrap()
                            .to_string(),
                        field_value,
                    );

                    if self.peek().kind != TokenKind::RightBrace {
                        self.expect(TokenKind::Comma)?;
                    }
                }

                let closing_brace = self.expect_punct(TokenKind::RightBrace)?;

                Ok(self.ast.new_object(fields, (token, closing_brace)))
            }
            TokenKind::Identifier => {
                if self.peek().kind == TokenKind::LeftParen {
                    self.parse_call_expr(token)
                } else if self.peek().kind == TokenKind::LeftBrace {
                    if self.is_context(&ParseContext::Normal) {
                        self.parse_struct_constructor(token)
                    } else {
                        Ok(self.ast.new_variable(token.clone(), token.literal()))
                    }
                } else {
                    Ok(self.ast.new_variable(token.clone(), token.literal()))
                }
            }
            TokenKind::LeftParen => {
                let expr = self.parse_expr()?;

                self.expect(TokenKind::RightParen)?;

                Ok(self.ast.new_parenthesized(expr))
            }
            TokenKind::String(s) => Ok(self.ast.new_string(s.clone(), token.clone())),
            TokenKind::Char(c) => Ok(self.ast.new_char(*c, token.clone())),
            _ => {
                Err(parser_error(
                    format!("Unexpected token: {}", token.kind.to_string()),
                    vec![(
                        token.span.clone(), None
                    )], vec![], None).into())
            }
        }
    }

    pub fn parse_call_expr(&mut self, callee: Token) -> Result<ExprId> {
        self.expect(TokenKind::LeftParen)?;

        let mut args = vec![];

        if self.peek().kind != TokenKind::RightParen {
            while self.peek().kind != TokenKind::RightParen && !self.is_eof() {
                let arg = self.parse_expr()?;

                args.push(arg);

                if self.peek().kind != TokenKind::RightParen {
                    self.expect(TokenKind::Comma)?;
                }
            }
        }

        self.expect(TokenKind::RightParen)?;

        Ok(self.ast.new_call(callee.literal(), args, callee))
    }

    pub fn parse_optional_type_annotation(&mut self) -> Result<Option<TypeAnnotation>> {
        if self.peek().kind == TokenKind::Colon {
            Ok(Some(self.parse_type_annotation(true)?))
        } else {
            Ok(None)
        }
    }

    pub fn parse_array(&mut self) -> Result<ExprId> {
        let mut elements = vec![];
        if self.peek().kind != TokenKind::RightBracket {
            while self.peek().kind != TokenKind::RightBracket && !self.is_eof() {
                let arg = self.parse_expr()?;

                elements.push(arg);

                if self.peek().kind != TokenKind::RightBracket {
                    self.expect(TokenKind::Comma)?;
                }
            }
        }

        self.expect(TokenKind::RightBracket)?;

        Ok(self.ast.new_array(elements))
    }

    pub fn parse_assignment(&mut self) -> Result<ExprId> {
        let expr = self.parse_binary_expression()?;
        if let Some(assign_op) = self.parse_assignment_operator() {
            self.consume();
            let right = self.parse_expr()?;

            let operator = AssignOperator::from_token_kind(assign_op);
            return Ok(self.ast.new_assignment(expr, operator, right));
        }
        // TODO: move this to binary expressions
        else if self.peek().kind == TokenKind::Catch {
            panic!("Catch not implemented");
        }

        Ok(expr)
    }

    fn parse_assignment_operator(&mut self) -> Option<TokenKind> {
        match self.peek().kind {
            TokenKind::Equals
            | TokenKind::PlusEquals
            | TokenKind::MinusEquals
            | TokenKind::MultiplyEquals
            | TokenKind::DivideEquals => Some(self.peek().kind.clone()),
            _ => None,
        }
    }
}

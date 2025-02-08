use crate::{
    parser::{
        PResult, PToken, PTokenKind, Parser,
        errors::{
            ElseBranchExpr, ElseIfAfterElse, InvalidLiteralSuffix, UnexpectedLiteralSuffix,
            UnexpectedToken,
        },
    },
    ptok,
};
use brim_ast::{
    Comptime, Else, If, Return, Try,
    expr::{BinOpAssociativity, BinOpKind, ConditionBranch, Expr, ExprKind, IfExpr, UnaryOp},
    token::{BinOpToken, Delimiter, Lit, LitKind, Orientation, TokenKind},
};
use brim_diagnostics::box_diag;
use brim_span::span::Span;

impl Parser {
    pub fn parse_expr(&mut self) -> PResult<Expr> {
        self.parse_assignment_expr()
    }

    fn parse_assignment_expr(&mut self) -> PResult<Expr> {
        let expr = self.parse_binary_expression()?;

        if let Some(op) = self.current().is_compound_assign() {
            self.advance();
            let right = self.parse_assignment_expr()?;
            return Ok(self.new_expr(
                expr.span.to(right.span),
                ExprKind::AssignOp(Box::new(expr), op, Box::new(right)),
            ));
        }

        if self.current().is_assign() {
            self.advance();

            let right = self.parse_assignment_expr()?;
            return Ok(self.new_expr(
                expr.span.to(right.span),
                ExprKind::Assign(Box::new(expr), Box::new(right)),
            ));
        }

        Ok(expr)
    }

    pub fn parse_binary_expression(&mut self) -> PResult<Expr> {
        let left = self.parse_unary_expr()?;
        self.parse_binary_expression_recurse(left, 0)
    }

    pub fn parse_binary_operator(&mut self) -> Option<BinOpKind> {
        match self.current().kind {
            TokenKind::BinOp(op) => Some(match op {
                BinOpToken::Plus => BinOpKind::Plus,
                BinOpToken::Minus => BinOpKind::Minus,
                BinOpToken::Star => BinOpKind::Multiply,
                BinOpToken::Slash => BinOpKind::Divide,
                BinOpToken::Percent => BinOpKind::Modulo,
                BinOpToken::Caret => BinOpKind::Caret,
                BinOpToken::And => BinOpKind::And,
                BinOpToken::Or => BinOpKind::Or,
                BinOpToken::ShiftLeft => BinOpKind::ShiftLeft,
                BinOpToken::ShiftRight => BinOpKind::ShiftRight,
                BinOpToken::Power => BinOpKind::Power,
            }),
            TokenKind::OrOr => Some(BinOpKind::OrOr),
            TokenKind::AndAnd => Some(BinOpKind::AndAnd),
            TokenKind::EqEq => Some(BinOpKind::EqEq),
            TokenKind::Ne => Some(BinOpKind::Ne),
            TokenKind::Lt => Some(BinOpKind::Lt),
            TokenKind::Le => Some(BinOpKind::Le),
            TokenKind::Gt => Some(BinOpKind::Gt),
            TokenKind::Ge => Some(BinOpKind::Ge),
            _ => None,
        }
    }

    pub fn parse_binary_expression_recurse(
        &mut self,
        mut left: Expr,
        precedence: u8,
    ) -> PResult<Expr> {
        while let Some(operator) = self.parse_binary_operator() {
            let operator_precedence = operator.precedence();
            if operator_precedence < precedence {
                break;
            }

            self.advance();
            let mut right = self.parse_unary_expr()?;

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

            left = self.new_expr(
                left.span.to(right.span),
                ExprKind::Binary(Box::new(left), operator, Box::new(right)),
            );
        }

        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> PResult<Expr> {
        if let Some(op) = self.peek_unary_op() {
            self.advance();
            let operand = self.parse_unary_expr()?;
            return Ok(self.new_expr(
                self.current().span.to(operand.span),
                ExprKind::Unary(op, Box::new(operand)),
            ));
        }

        self.parse_access_expr()
    }

    pub fn parse_access_expr(&mut self) -> PResult<Expr> {
        let mut primary = self.parse_primary_expr()?;

        loop {
            match self.current().kind {
                TokenKind::Dot => {
                    self.advance();
                    let ident = self.parse_ident()?;
                    primary = self.new_expr(
                        primary.span.to(ident.span),
                        ExprKind::Field(Box::new(primary), ident),
                    );
                }
                TokenKind::Delimiter(Delimiter::Bracket, Orientation::Open) => {
                    self.advance();
                    let index = self.parse_expr()?;
                    self.expect_cbracket()?;
                    primary = self.new_expr(
                        primary.span.to(index.span),
                        ExprKind::Index(Box::new(primary), Box::new(index)),
                    );
                }
                _ => break,
            }
        }

        Ok(primary)
    }

    pub fn validate_suffix(&mut self, lit: Lit) -> PResult<()> {
        if let Some(sym) = lit.suffix {
            let suffix = sym.to_string();
            let span = sym.span;

            // Integer can be turned into a float but not the other way around
            let valid_suffixes = match lit.kind {
                LitKind::Integer => vec![
                    "u8", "u16", "u32", "u64", "usize", "i8", "i16", "i32", "i64", "isize", "f32",
                    "f64",
                ],
                LitKind::Float => vec!["f32", "f64"],
                _ => vec![],
            };
            let msg = if valid_suffixes.is_empty() {
                "This literal doesn't expect any suffix".to_string()
            } else {
                format!(
                    "This literal expects one of the following suffixes: {}",
                    valid_suffixes.join(", ")
                )
            };

            // This means that the suffix is not expected in the literal
            if valid_suffixes.is_empty() {
                self.emit(UnexpectedLiteralSuffix {
                    lit,
                    span: (span, self.file),
                    note: msg,
                });
            } else if !valid_suffixes.contains(&suffix.as_str()) {
                self.emit(InvalidLiteralSuffix {
                    lit,
                    span: (span, self.file),
                    note: msg,
                });
            }
        }

        Ok(())
    }

    pub fn parse_primary_expr(&mut self) -> PResult<Expr> {
        match self.current().kind {
            TokenKind::Literal(lit) => {
                self.advance();
                self.validate_suffix(lit)?;

                Ok(self.new_expr(self.prev().span, ExprKind::Literal(lit)))
            }
            TokenKind::Delimiter(Delimiter::Paren, Orientation::Open) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect_cparen()?;
                Ok(self.new_expr(expr.span, ExprKind::Paren(Box::new(expr))))
            }
            TokenKind::Delimiter(Delimiter::Bracket, Orientation::Open) => {
                self.advance();
                let mut elements = Vec::new();
                while !self.is_paren(Orientation::Close) {
                    elements.push(self.parse_expr()?);
                    if !self.eat(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect_cbracket()?;
                Ok(self.new_expr(self.current().span, ExprKind::Array(elements)))
            }
            TokenKind::At => {
                let span = self.current().span;
                self.advance();

                let ident = self.parse_ident()?;

                self.expect_oparen()?;
                let mut args = Vec::new();
                while !self.is_paren(Orientation::Close) {
                    args.push(self.parse_expr()?);
                    if !self.eat(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect_cparen()?;

                Ok(self.new_expr(span.to(self.prev().span), ExprKind::Builtin(ident, args)))
            }
            TokenKind::Ident(sym) => {
                if self.eat_keyword(ptok!(Return)) {
                    let expr = self.parse_expr()?;
                    Ok(self.new_expr(
                        self.current().span.to(expr.span),
                        ExprKind::Return(Box::new(expr)),
                    ))
                } else if self.eat_keyword(ptok!(If)) {
                    self.parse_if()
                } else if self.eat_keyword(ptok!(Comptime)) {
                    let block = self.parse_block(true)?;
                    let expr = self.new_expr(block.span, ExprKind::Block(block));

                    Ok(self.new_expr(self.current().span, ExprKind::Comptime(Box::new(expr))))
                } else {
                    let span = self.current().span;
                    let ident = self.parse_ident()?;

                    if self.is_paren(Orientation::Open) {
                        self.advance();

                        let mut args = Vec::new();
                        while !self.is_paren(Orientation::Close) {
                            args.push(self.parse_expr()?);
                            if !self.eat(TokenKind::Comma) {
                                break;
                            }
                        }

                        self.expect_cparen()?;
                        let var = self.new_expr(span, ExprKind::Var(ident));
                        Ok(self.new_expr(
                            span.to(self.prev().span),
                            ExprKind::Call(Box::new(var), args),
                        ))
                    } else {
                        Ok(self.new_expr(ident.span, ExprKind::Var(ident)))
                    }
                }
            }
            _ => {
                box_diag!(UnexpectedToken {
                    found: self.current().kind,
                    span: (self.current().span, self.file),
                })
            }
        }
    }

    pub fn parse_if(&mut self) -> PResult<Expr> {
        let span = self.prev().span;
        let condition = self.parse_expr()?;
        let then_block = self.parse_block(true)?;

        let mut else_block: Option<Expr> = None;
        let mut else_ifs: Vec<ConditionBranch> = Vec::new();

        while self.eat_keyword(ptok!(Else)) {
            if self.eat_keyword(ptok!(If)) {
                let span = self.prev().span;
                let condition = self.parse_expr()?;
                if let Some(else_block) = &else_block {
                    self.emit(ElseIfAfterElse {
                        else_if: (span.to(self.current().span), self.file),
                        else_block: (else_block.span, self.file),
                    });
                }
                let block = self.parse_block(true)?;

                else_ifs.push(ConditionBranch {
                    span: span.to(block.span),
                    condition: Box::new(condition),
                    block: Box::new(self.new_expr(block.span, ExprKind::Block(block))),
                });
            } else {
                let span = self.current().span;

                if self.current().kind != TokenKind::Delimiter(Delimiter::Brace, Orientation::Open)
                {
                    // we eat until we find the brace of the opening of the else block
                    let span_of_brace = self.eat_until_brace(Orientation::Open);

                    self.emit(ElseBranchExpr {
                        span: (span.to(self.prev().span), self.file),
                    });
                }
                let block = self.parse_block(true)?;
                else_block = Some(self.new_expr(block.span, ExprKind::Block(block)));
            };
        }

        let then_block = self.new_expr(then_block.span, ExprKind::Block(then_block));
        Ok(self.new_expr(
            span,
            ExprKind::If(IfExpr {
                span,
                condition: Box::new(condition),
                then_block: Box::new(then_block),
                else_block: else_block.map(Box::new),
                else_ifs,
            }),
        ))
    }

    fn peek_unary_op(&mut self) -> Option<UnaryOp> {
        match self.current().kind {
            TokenKind::BinOp(BinOpToken::Minus) => Some(UnaryOp::Minus),
            TokenKind::Bang => Some(UnaryOp::Not),
            TokenKind::BinOp(BinOpToken::Star) => Some(UnaryOp::Deref),
            TokenKind::Ident(ident) if ident == Try => Some(UnaryOp::Try),
            _ => None,
        }
    }

    pub fn new_expr(&mut self, span: Span, kind: ExprKind) -> Expr {
        Expr {
            id: self.new_id(),
            span,
            kind,
        }
    }
}

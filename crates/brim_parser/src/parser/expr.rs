use crate::parser::PToken;
use brim::expr::{BinOpAssociativity, BinOpKind, ConstExpr, Expr, ExprKind, UnaryOp};
use brim::{box_diag, NodeId, Return, Try};
use brim::item::Ident;
use brim::span::Span;
use brim::token::{BinOpToken, Delimiter, Orientation, TokenKind};
use brim::ty::{Const, Ty};
use crate::parser::{PResult, PTokenKind, Parser};
use crate::parser::errors::UnexpectedToken;
use crate::ptok;

impl<'a> Parser<'a> {
    pub fn parse_const_expr(&mut self) -> PResult<'a, ConstExpr> {
        Ok(ConstExpr {
            expr: Box::new(self.parse_expr()?),
            id: NodeId::max(),
        })
    }

    pub fn parse_expr(&mut self) -> PResult<'a, Expr> {
        self.parse_assignment_expr()
    }

    fn parse_assignment_expr(&mut self) -> PResult<'a, Expr> {
        let expr = self.parse_binary_expression()?;

        if let Some(op) = self.current().is_compound_assign() {
            self.advance();
            let right = self.parse_assignment_expr()?;
            return Ok(self.new_expr(expr.span.to(right.span), ExprKind::AssignOp(Box::new(expr), op, Box::new(right))));
        }

        if self.current().is_assign() {
            self.advance();
        
            let right = self.parse_assignment_expr()?;
            return Ok(self.new_expr(expr.span.to(right.span), ExprKind::Assign(Box::new(expr), Box::new(right))));
        }

        Ok(expr)
    }

    pub fn parse_binary_expression(&mut self) -> PResult<'a, Expr> {
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
    ) -> PResult<'a, Expr> {
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

            left = self.new_expr(left.span.to(right.span), ExprKind::Binary(Box::new(left), operator, Box::new(right)));
        }

        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> PResult<'a, Expr> {
        if let Some(op) = self.peek_unary_op() {
            self.advance();
            let operand = self.parse_unary_expr()?;
            return Ok(self.new_expr(self.current().span.to(operand.span), ExprKind::Unary(op, Box::new(operand))));
        }

        self.parse_access_expr()
    }

    pub fn parse_access_expr(&mut self) -> PResult<'a, Expr> {
        let mut primary = self.parse_primary_expr()?;

        loop {
            match self.current().kind {
                TokenKind::Dot => {
                    self.advance();
                    let ident = self.parse_ident()?;
                    primary = self.new_expr(primary.span.to(ident.span), ExprKind::Field(Box::new(primary), ident));
                }
                TokenKind::Delimiter(Delimiter::Bracket, Orientation::Open) => {
                    self.advance();
                    let index = self.parse_expr()?;
                    self.expect_cbracket()?;
                    primary = self.new_expr(primary.span.to(index.span), ExprKind::Index(Box::new(primary), Box::new(index)));
                }
                _ => break,
            }
        }

        Ok(primary)
    }

    pub fn parse_primary_expr(&mut self) -> PResult<'a, Expr> {
        match self.current().kind {
            TokenKind::Literal(lit) => {
                let span = self.advance().span;
                Ok(self.new_expr(span, ExprKind::Literal(lit)))
            }
            TokenKind::Delimiter(Delimiter::Paren, Orientation::Open) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect_cparen()?;
                Ok(self.new_expr(expr.span, ExprKind::Paren(Box::new(expr))))
            }
            TokenKind::Ident(sym) => {
                if self.eat_keyword(ptok!(Return)) {
                    let expr = self.parse_expr()?;
                    Ok(self.new_expr(self.current().span.to(expr.span), ExprKind::Return(Box::new(expr))))
                } else {
                    let ident = self.parse_ident()?;

                    Ok(self.new_expr(ident.span, ExprKind::Var(ident)))
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

    fn peek_unary_op(&mut self) -> Option<UnaryOp> {
        match self.current().kind {
            TokenKind::BinOp(BinOpToken::Minus) => Some(UnaryOp::Minus),
            TokenKind::Bang => Some(UnaryOp::Not),
            TokenKind::BinOp(BinOpToken::Star) => Some(UnaryOp::Deref),
            TokenKind::Ident(ident) if ident == Try => Some(UnaryOp::Try),
            _ => None,
        }
    }

    pub fn new_expr(&self, span: Span, kind: ExprKind) -> Expr {
        Expr {
            id: NodeId::max(),
            span,
            kind,
        }
    }
}
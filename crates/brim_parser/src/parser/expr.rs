use super::errors::{
    ExpectedColonInTernaryError, ExpectedStringLiteralError, ExpectedTypeAfterConst,
    IfAsExpressionError,
};
use crate::{
    parser::{
        PResult, PToken, PTokenKind, Parser, ParsingContext,
        errors::{
            ElseBranchExpr, ElseIfAfterElse, EmptyMatchExpressionError, InvalidLiteralSuffix,
            MissingFatArrowError, MultipleElseArmsError, UnexpectedLiteralSuffix, UnexpectedToken,
            UnexpectedTokenInMatch,
        },
    },
    ptok,
};
use brim_ast::{
    Comptime, Const, Else, If, Match, Return, Try,
    expr::{BinOpAssociativity, BinOpKind, Expr, ExprKind, Match, MatchArm, UnaryOp},
    item::{Block, Ident},
    stmts::{ConditionBranch, IfStmt, StmtKind},
    token::{BinOpToken, Delimiter, Lit, LitKind, Orientation, TokenKind},
    ty::{Ty, TyKind},
};
use brim_span::span::Span;
use indexmap::IndexMap;
use tracing::debug;

impl Parser {
    pub fn parse_expr(&mut self) -> Expr {
        self.parse_assignment_expr()
    }

    fn parse_assignment_expr(&mut self) -> Expr {
        let expr = self.parse_ternary_expr();

        if let Some(op) = self.current().is_compound_assign() {
            debug!("Found compound assignment operator: {:?}", op);

            self.advance();
            let right = self.parse_assignment_expr();
            return self.new_expr(
                expr.span.to(right.span),
                ExprKind::AssignOp(Box::new(expr), op, Box::new(right)),
            );
        }

        if self.current().is_assign() {
            debug!("Found assignment operator");
            self.advance();
            let span = self.prev().span;
            let right = self.parse_assignment_expr();
            return self.new_expr(
                expr.span.to(right.span),
                ExprKind::Assign(Box::new(expr), Box::new(right)),
            );
        }

        expr
    }

    fn parse_ternary_expr(&mut self) -> Expr {
        let condition = self.parse_binary_expression();

        if self.current().kind == TokenKind::QuestionMark {
            debug!("Found ternary operator");
            self.advance();

            let true_expr = self.parse_expr();

            if self.current().kind == TokenKind::Colon {
                self.advance();
                let false_expr = self.parse_assignment_expr();

                return self.new_expr(
                    condition.span.to(false_expr.span),
                    ExprKind::Ternary(
                        Box::new(condition),
                        Box::new(true_expr),
                        Box::new(false_expr),
                    ),
                );
            } else {
                self.dcx.emit_impl(ExpectedColonInTernaryError {
                    span: (self.current().span, self.file),
                });

                let dummy = self.dummy_block();
                return self.new_expr(
                    condition.span.to(true_expr.span),
                    ExprKind::Ternary(Box::new(condition), Box::new(true_expr), Box::new(dummy)),
                );
            }
        }

        condition
    }

    pub fn parse_binary_expression(&mut self) -> Expr {
        let left = self.parse_unary_expr();
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
            TokenKind::Ident(sym) if sym.to_string() == "orelse" => Some(BinOpKind::OrElse),
            _ => None,
        }
    }

    pub fn parse_binary_expression_recurse(&mut self, mut left: Expr, precedence: u8) -> Expr {
        while let Some(operator) = self.parse_binary_operator() {
            let operator_precedence = operator.precedence();
            if operator_precedence < precedence {
                break;
            }

            self.advance();

            let mut right = self.parse_unary_expr();

            while let Some(next_operator) = self.parse_binary_operator() {
                let next_precedence = next_operator.precedence();

                if next_precedence > operator_precedence
                    || (next_precedence == operator_precedence
                        && next_operator.associativity() == BinOpAssociativity::Right)
                {
                    right = self.parse_binary_expression_recurse(right, next_precedence);
                } else {
                    break;
                }
            }

            left = self.new_expr(
                left.span.to(right.span),
                ExprKind::Binary(Box::new(left), operator, Box::new(right)),
            );
        }

        left
    }

    fn parse_unary_expr(&mut self) -> Expr {
        let span = self.current().span;
        if let Some(op) = self.peek_unary_op() {
            debug!("Found unary operator: {:?}", op);

            self.advance();
            let operand = self.parse_unary_expr();
            return self
                .new_expr(span.to(operand.span), ExprKind::Unary(span, op, Box::new(operand)));
        }

        self.parse_access_expr()
    }

    pub fn parse_access_expr(&mut self) -> Expr {
        let mut primary = self.parse_primary_expr();

        loop {
            match self.current().kind {
                TokenKind::Delimiter(Delimiter::Bracket, Orientation::Open) => {
                    self.advance();
                    let index = self.parse_expr();
                    self.expect_cbracket();
                    primary = self.new_expr(
                        primary.span.to(index.span),
                        ExprKind::Index(Box::new(primary), Box::new(index)),
                    );

                    debug!("Parsed index expression");
                }
                TokenKind::Bang => {
                    self.advance();
                    primary = self.new_expr(
                        primary.span.to(self.prev().span),
                        ExprKind::Unwrap(Box::new(primary)),
                    );
                }
                _ => break,
            }
        }

        primary
    }

    pub fn parse_primary_expr(&mut self) -> Expr {
        match self.current().kind {
            TokenKind::Literal(lit) => {
                let span = self.current().span;
                self.advance();
                self.validate_suffix(lit);

                let span = span.to(self.prev().span);
                self.new_expr(span, ExprKind::Literal(lit, span))
            }
            TokenKind::Ident(_) => {
                if self.eat_keyword(ptok!(Return)) {
                    let span = self.prev().span;
                    let expr = self.parse_expr();

                    self.new_expr(expr.span, ExprKind::Return(Box::new(expr), span))
                } else if self.eat_keyword(ptok!(If)) {
                    self.dcx
                        .emit_impl(IfAsExpressionError { span: (self.current().span, self.file) });

                    self.dummy_block()
                } else if self.eat_keyword(ptok!(Comptime)) {
                    let block = self.parse_block(true);
                    let expr = self.new_expr(block.span, ExprKind::Block(block));

                    debug!("Parsed comptime expression");
                    self.new_expr(self.current().span, ExprKind::Comptime(Box::new(expr)))
                } else if self.is_keyword(ptok!(Match)) {
                    self.parse_match_expression()
                } else if self.current().is_keyword(Const) {
                    if let Some(ty) = self.parse_ty_without_ident() {
                        self.new_expr(ty.span, ExprKind::Type(Box::new(ty)))
                    } else {
                        self.dcx.emit_impl(ExpectedTypeAfterConst {
                            found: self.current().kind,
                            span: (self.current().span, self.file),
                        });

                        self.dummy_block()
                    }
                } else {
                    let span = self.current().span;
                    let ident = self.parse_ident();

                    if self.is_paren(Orientation::Open) {
                        self.expect_oparen();
                        let mut args = Vec::new();
                        while !self.is_paren(Orientation::Close) {
                            args.push(self.parse_expr());

                            if !self.eat(TokenKind::Comma) {
                                break;
                            }
                        }

                        self.expect_cparen();
                        let var = self.new_expr(span, ExprKind::Var(ident));

                        debug!("Parsed function call expression");
                        self.new_expr(
                            span.to(self.prev().span),
                            ExprKind::Call(Box::new(var), args),
                        )
                    } else if self.is_brace(Orientation::Open) {
                        if self.parsing_ctx == ParsingContext::IfStatement
                            || self.parsing_ctx == ParsingContext::Match
                        {
                            return self.new_expr(span, ExprKind::Var(ident));
                        }

                        self.advance();

                        let mut fields: IndexMap<Ident, Expr> = IndexMap::new();
                        let generics = self.parse_argument_generics();

                        while !self.is_brace(Orientation::Close) {
                            self.expect(TokenKind::Dot);
                            let ident = self.parse_ident();
                            self.expect(TokenKind::Eq);
                            let field = self.parse_expr();

                            fields.insert(ident, field);

                            if !self.eat(TokenKind::Comma) {
                                break;
                            }
                        }

                        self.expect_cbrace();

                        self.new_expr(
                            span.to(self.prev().span),
                            ExprKind::StructConstructor(ident, generics, fields),
                        )
                    } else {
                        if let Some(primitive) = self.is_primitive(ident) {
                            let ty =
                                Ty { span, kind: TyKind::Primitive(primitive), id: self.new_id() };
                            return self.new_expr(span, ExprKind::Type(Box::new(ty)));
                        }

                        let generics = self.parse_argument_generics();
                        if !generics.params.is_empty() {
                            let ty = Ty {
                                span,
                                kind: TyKind::Ident { ident, generics },
                                id: self.new_id(),
                            };
                            return self.new_expr(span, ExprKind::Type(Box::new(ty)));
                        }

                        if self.eat(TokenKind::DoubleColon) {
                            let mut path = vec![ident];

                            loop {
                                if let TokenKind::Ident(_) = self.current().kind {
                                    if self.next().kind
                                        == TokenKind::Delimiter(Delimiter::Paren, Orientation::Open)
                                    {
                                        let expr = self.parse_expr();

                                        return self.new_expr(
                                            span,
                                            ExprKind::StaticAccess(path, Box::new(expr)),
                                        );
                                    }

                                    path.push(self.parse_ident());
                                } else {
                                    break;
                                }

                                if !self.eat(TokenKind::DoubleColon) {
                                    break;
                                }
                            }

                            debug!("Parsed path expression");
                            self.new_expr(span, ExprKind::Path(path))
                        } else if self.eat(TokenKind::Dot) {
                            let mut idents = vec![ident];

                            while let TokenKind::Ident(_) = self.current().kind {
                                idents.push(self.parse_ident());

                                if !self.eat(TokenKind::Dot) {
                                    break;
                                }
                            }

                            if self.is_paren(Orientation::Open) {
                                self.expect_oparen();
                                let mut args = vec![];
                                while !self.is_paren(Orientation::Close) {
                                    args.push(self.parse_expr());
                                    if !self.eat(TokenKind::Comma) {
                                        break;
                                    }
                                }
                                self.expect_cparen();

                                let call_span = span.to(self.prev().span);
                                let ex = ExprKind::Call(
                                    Box::new(
                                        self.new_expr(idents[0].span, ExprKind::Var(idents[0])),
                                    ),
                                    args,
                                );
                                let call_expr = self.new_expr(call_span, ex);
                                self.new_expr(
                                    span,
                                    ExprKind::MethodCall(idents, Box::new(call_expr)),
                                )
                            } else {
                                self.new_expr(span, ExprKind::Field(idents))
                            }
                        } else {
                            debug!("Parsed variable expression: {}", ident);
                            self.new_expr(span, ExprKind::Var(ident))
                        }
                    }
                }
            }
            TokenKind::Delimiter(Delimiter::Paren, Orientation::Open) => {
                debug!("Found parenthesized expression");

                self.expect_oparen();
                let expr = self.parse_expr();
                self.expect_cparen();

                self.new_expr(expr.span, ExprKind::Paren(Box::new(expr)))
            }
            TokenKind::Delimiter(Delimiter::Bracket, Orientation::Open) => {
                let span_start = self.current().span;

                self.advance();
                let mut elements = Vec::new();
                while !self.is_paren(Orientation::Close) {
                    elements.push(self.parse_expr());
                    if !self.eat(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect_cbracket();

                debug!("Parsed array expression");
                self.new_expr(span_start.to(self.current().span), ExprKind::Array(elements))
            }
            TokenKind::BinOp(BinOpToken::And) | TokenKind::BinOp(BinOpToken::Star) => {
                if let Some(ty) = self.parse_ty_without_ident() {
                    return self.new_expr(ty.span, ExprKind::Type(Box::new(ty)));
                }

                todo!("error?")
                // box_diag!(UnexpectedToken {
                //     found: self.current().kind,
                //     span: (self.current().span, self.file),
                // })
            }
            TokenKind::At => {
                let span = self.current().span;
                self.advance();

                let ident = self.parse_ident();
                let str = ident.to_string();

                self.expect_oparen();
                let mut args = Vec::new();
                while !self.is_paren(Orientation::Close) {
                    if str == "anyCast" || str == "cast" {
                        if args.len() == 1 {
                            let ty = self.parse_type();
                            args.push(self.new_expr(ty.span, ExprKind::Type(Box::new(ty))));
                        } else {
                            args.push(self.parse_expr());
                        }
                    } else {
                        args.push(self.parse_expr());
                    }

                    if !self.eat(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect_cparen();

                debug!("Parsed builtin function with name: {}", ident);
                self.new_expr(span.to(self.prev().span), ExprKind::Builtin(ident, args))
            }
            _ => {
                self.dcx.emit_impl(UnexpectedToken {
                    found: self.current().kind,
                    span: (self.current().span, self.file),
                });

                self.dummy_block()
            }
        }
    }

    pub fn validate_suffix(&mut self, lit: Lit) {
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
                self.emit(UnexpectedLiteralSuffix { lit, span: (span, self.file), note: msg });
            } else if !valid_suffixes.contains(&suffix.as_str()) {
                self.emit(InvalidLiteralSuffix { lit, span: (span, self.file), note: msg });
            }
        }
    }

    fn expect_fat_arrow(&mut self) {
        if !self.eat(TokenKind::FatArrow) {
            self.dcx.emit_impl(MissingFatArrowError {
                found: self.current().kind,
                span: (self.current().span, self.file),
            });
        }
    }

    pub fn parse_match(&mut self) -> Match {
        self.parsing_ctx = ParsingContext::Match;
        if !self.eat_keyword(ptok!(Match)) {
            self.dcx.emit_impl(UnexpectedTokenInMatch {
                found: self.current().kind,
                span: (self.current().span, self.file),
            });
        }

        let start_span = self.prev().span;
        let subject_expr = self.parse_expr();

        if !self.is_brace(Orientation::Open) {
            self.dcx.emit_impl(UnexpectedTokenInMatch {
                found: self.current().kind,
                span: (self.current().span, self.file),
            });
        } else {
            self.advance();
        }

        let mut match_arms = Vec::new();
        let mut has_else_arm = false;

        while !self.is_brace(Orientation::Close) {
            if self.eat_keyword(ptok!(Else)) {
                if has_else_arm {
                    self.dcx
                        .emit_impl(MultipleElseArmsError { span: (self.prev().span, self.file) });
                }

                self.expect_fat_arrow();
                let else_arm = self.parse_else_arm();
                match_arms.push(else_arm);
                has_else_arm = true;
                break;
            }

            let arm = self.parse_match_arm();
            match_arms.push(arm);

            self.eat_possible(TokenKind::Comma);

            if has_else_arm {
                break;
            }
        }

        if !self.is_brace(Orientation::Close) {
            self.dcx.emit_impl(UnexpectedTokenInMatch {
                found: self.current().kind,
                span: (self.current().span, self.file),
            });
        } else {
            self.advance();
        }

        if match_arms.is_empty() {
            self.dcx.emit_impl(EmptyMatchExpressionError { span: (start_span, self.file) });
        }

        self.parsing_ctx = ParsingContext::Normal;
        Match {
            expr: Box::new(subject_expr),
            arms: match_arms,
            span: start_span.to(self.prev().span),
        }
    }

    pub fn parse_match_expression(&mut self) -> Expr {
        let mt = self.parse_match();
        self.new_expr(mt.span, ExprKind::Match(mt))
    }

    fn parse_match_arm(&mut self) -> MatchArm {
        let pattern = self.parse_expr();

        self.expect_fat_arrow();
        let arm_expr = self.parse_expr_or_block();

        self.eat_possible(TokenKind::Comma);

        MatchArm::Case(pattern, arm_expr)
    }

    fn parse_else_arm(&mut self) -> MatchArm {
        let else_expr = self.parse_expr_or_block();

        MatchArm::Else(else_expr)
    }

    fn parse_expr_or_block(&mut self) -> Expr {
        if self.is_brace(Orientation::Open) {
            self.advance();
            let block = self.parse_block(true);
            self.new_expr(block.span, ExprKind::Block(block))
        } else {
            self.parse_expr()
        }
    }

    pub fn expect_str_literal(&mut self) -> String {
        if let TokenKind::Literal(lit) = self.current().kind {
            if let LitKind::Str = lit.kind {
                self.advance();
                return lit.symbol.to_string();
            }
        }

        self.dcx.emit_impl(ExpectedStringLiteralError { span: (self.current().span, self.file) });

        String::new()
    }

    pub fn parse_if(&mut self) -> StmtKind {
        self.parsing_ctx = ParsingContext::IfStatement;
        let span = self.prev().span;
        let condition = self.parse_expr();
        self.parsing_ctx = ParsingContext::Normal;
        let then_block = self.parse_block(true);
        self.parsing_ctx = ParsingContext::IfStatement;

        let mut else_block: Option<Expr> = None;
        let mut else_ifs: Vec<ConditionBranch> = Vec::new();

        while self.eat_keyword(ptok!(Else)) {
            if self.eat_keyword(ptok!(If)) {
                let span = self.prev().span;
                let condition = self.parse_expr();
                if let Some(else_block) = &else_block {
                    self.emit(ElseIfAfterElse {
                        else_if: (span.to(self.current().span), self.file),
                        else_block: (else_block.span, self.file),
                    });
                }
                self.parsing_ctx = ParsingContext::Normal;
                let block = self.parse_block(true);
                self.parsing_ctx = ParsingContext::IfStatement;

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
                    let _ = self.eat_until_brace(Orientation::Open);

                    self.emit(ElseBranchExpr { span: (span.to(self.prev().span), self.file) });
                }
                self.parsing_ctx = ParsingContext::Normal;
                let block = self.parse_block(true);
                self.parsing_ctx = ParsingContext::IfStatement;
                else_block = Some(self.new_expr(block.span, ExprKind::Block(block)));
            };
        }

        let then_block = self.new_expr(then_block.span, ExprKind::Block(then_block));

        self.parsing_ctx = ParsingContext::Normal;
        debug!("Parsed if expression");
        StmtKind::If(IfStmt {
            span,
            condition: Box::new(condition),
            then_block: Box::new(then_block),
            else_block: else_block.map(Box::new),
            else_ifs,
        })
    }

    fn peek_unary_op(&mut self) -> Option<UnaryOp> {
        match self.current().kind {
            TokenKind::BinOp(BinOpToken::Minus) => Some(UnaryOp::Minus),
            TokenKind::Bang => Some(UnaryOp::Not),
            TokenKind::BinOp(BinOpToken::Star) => Some(UnaryOp::Deref),
            TokenKind::BinOp(BinOpToken::And) => Some(UnaryOp::Ref),
            TokenKind::Ident(ident) if ident == Try => Some(UnaryOp::Try),
            _ => None,
        }
    }

    pub fn new_expr(&mut self, span: Span, kind: ExprKind) -> Expr {
        Expr { id: self.new_id(), span, kind }
    }
}

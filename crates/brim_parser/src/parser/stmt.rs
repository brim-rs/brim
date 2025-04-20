use super::{PResult, Parser};
use crate::{
    parser::{PToken, PTokenKind, errors::InvalidVariableInit},
    ptok,
};
use brim_ast::{
    If, Let, Match,
    item::Block,
    stmts::{Let, Stmt, StmtKind},
    token::{Delimiter, Orientation, TokenKind},
};

impl Parser {
    pub fn parse_block(&mut self, eat_braces: bool) -> Block {
        let mut stmts = vec![];
        let span_start = self.current().span;
        if eat_braces {
            self.expect_obrace();
        }

        while !self.current().is(TokenKind::Delimiter(Delimiter::Brace, Orientation::Close)) {
            let stmt = self.parse_stmt();
            self.eat_semis();

            stmts.push(stmt);
        }

        let span_end = self.current().span;
        if eat_braces {
            self.expect_cbrace();
        }

        Block { id: self.new_id(), stmts, span: span_start.to(span_end) }
    }

    pub fn parse_stmt(&mut self) -> Stmt {
        let start = self.current().span;

        let kind = if self.current().is_keyword(Let) {
            let stmt = self.parse_let();

            StmtKind::Let(stmt)
        } else if self.current().is_keyword(If) {
            self.advance();

            self.parse_if()
        } else if self.current().is_keyword(Match) {
            StmtKind::Match(self.parse_match())
        } else {
            let expr = self.parse_expr();
            StmtKind::Expr(expr)
        };

        self.eat_semis();

        Stmt { id: self.new_id(), kind, span: start.to(self.current().span) }
    }

    pub fn parse_let(&mut self) -> Let {
        let span = self.current().span;
        self.eat_keyword(ptok!(Let));
        let keyword = self.prev().span;

        let ident = self.parse_ident();
        let ty = if self.eat(TokenKind::Colon) { Some(self.parse_type()) } else { None };

        let value = if self.eat(TokenKind::Eq) {
            Some(self.parse_expr())
        } else if let Some(op) = self.current().is_compound_assign() {
            self.dcx.emit_impl(InvalidVariableInit {
                found: op,
                span: (self.current().span, self.file),
            });

            self.advance();
            Some(self.parse_expr())
        } else {
            None
        };

        Let { id: self.new_id(), ident, ty, value, span: span.to(self.prev().span), keyword }
    }
}

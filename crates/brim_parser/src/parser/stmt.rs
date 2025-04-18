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
use brim_diagnostics::box_diag;

impl Parser {
    pub fn parse_block(&mut self, eat_braces: bool) -> PResult<Block> {
        let mut stmts = vec![];
        let span_start = self.current().span;
        if eat_braces {
            self.expect_obrace()?;
        }

        while !self.current().is(TokenKind::Delimiter(Delimiter::Brace, Orientation::Close)) {
            let stmt = self.parse_stmt()?;
            self.eat_semis();

            stmts.push(stmt);
        }

        let span_end = self.current().span;
        if eat_braces {
            self.expect_cbrace()?;
        }

        Ok(Block { id: self.new_id(), stmts, span: span_start.to(span_end) })
    }

    pub fn parse_stmt(&mut self) -> PResult<Stmt> {
        let start = self.current().span;

        let kind = if self.current().is_keyword(Let) {
            let stmt = self.parse_let()?;

            Ok(StmtKind::Let(stmt))
        } else if self.current().is_keyword(If) {
            self.advance();

            Ok(self.parse_if()?)
        } else if self.current().is_keyword(Match) {
            Ok(StmtKind::Match(self.parse_match()?))
        } else {
            let expr = self.parse_expr()?;

            Ok(StmtKind::Expr(expr))
        };

        self.eat_semis();

        Ok(Stmt { id: self.new_id(), kind: kind?, span: start.to(self.prev().span) })
    }

    pub fn parse_let(&mut self) -> PResult<Let> {
        let span = self.current().span;
        self.eat_keyword(ptok!(Let));
        let keyword = self.prev().span;

        let ident = self.parse_ident()?;
        let mut colon = None;
        let ty = if self.eat(TokenKind::Colon) {
            colon = Some(self.prev().span);
            Some(self.parse_type()?)
        } else {
            None
        };

        let mut eq = None;
        let value = if self.eat(TokenKind::Eq) {
            eq = Some(self.prev().span);
            Some(self.parse_expr()?)
        } else if let Some(op) = self.current().is_compound_assign() {
            box_diag!(InvalidVariableInit { found: op, span: (self.current().span, self.file) })
        } else {
            None
        };

        Ok(Let {
            id: self.new_id(),
            ident,
            ty,
            value,
            span: span.to(self.prev().span),
            keyword,
            colon,
            eq,
        })
    }
}

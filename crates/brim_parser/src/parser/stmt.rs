use crate::parser::PToken;
use crate::parser::PTokenKind;
use anyhow::Result;
use brim::{NodeId, item::Block, stmts::Stmt, token::{Delimiter, Orientation, TokenKind}, Let, box_diag};
use brim::stmts::{Let, StmtKind};
use crate::parser::errors::InvalidVariableInit;
use crate::ptok;
use super::{PResult, Parser};

impl<'a> Parser<'a> {
    pub fn parse_block(&mut self) -> PResult<'a, Block> {
        let mut stmts = vec![];
        let span_start = self.current().span;

        while !self.current().is(TokenKind::Delimiter(Delimiter::Brace, Orientation::Close)) {
            let stmt = self.parse_stmt()?;
            self.eat_semis();
            
            stmts.push(stmt);
        }

        Ok(Block {
            id: NodeId::max(),
            stmts,
            span: span_start.to(self.prev().span),
        })
    }

    pub fn parse_stmt(&mut self) -> PResult<'a, Stmt> {
        let start = self.current().span;

        let kind = if self.current().is_keyword(Let) {
            let stmt = self.parse_let()?;
            
            Ok(StmtKind::Let(stmt))
        } else {
            todo!()
        };
        self.eat_semis();
        
        Ok(Stmt {
            id: NodeId::max(),
            kind: kind?,
            span: start.to(self.prev().span),
        })
    }

    pub fn parse_let(&mut self) -> PResult<'a, Let> {
        let span = self.current().span;
        self.eat_keyword(ptok!(Let));

        let ident = self.parse_ident()?;

        let ty = if self.eat(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let value = if self.eat(TokenKind::Eq) {
            Some(self.parse_expr()?)
        } else if let Some(op) = self.current().is_assign() {
            box_diag!(InvalidVariableInit {
                found: op,
                span: (self.current().span, self.file),
            })
        } else {
            None
        };
        
        Ok(Let {
            id: NodeId::max(),
            ident,
            ty,
            value,
            span: span.to(self.prev().span),
        })
    }
}

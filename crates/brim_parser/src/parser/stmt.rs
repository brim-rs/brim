use anyhow::Result;
use brim::{
    NodeId,
    item::Block,
    stmts::Stmt,
    token::{Delimiter, Orientation, TokenKind},
};

use super::{PResult, Parser};

impl<'a> Parser<'a> {
    pub fn parse_block(&mut self) -> PResult<'a, Block> {
        let mut stmts = vec![];
        let span_start = self.current().span;

        while !self.eat(TokenKind::Delimiter(Delimiter::Brace, Orientation::Close)) {
            let stmt = self.parse_stmt()?;

            self.eat_semis();
        }

        Ok(Block {
            id: NodeId::max(),
            stmts,
            span: span_start.to(self.prev().span),
        })
    }
}

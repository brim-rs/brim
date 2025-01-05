use brim::item::Item;
use crate::parser::Parser;
use anyhow::Result;
use brim::token::TokenKind;

impl<'a> Parser<'a> {
    pub fn parse_item(&mut self) -> Result<Option<Item>> {
        if self.token_cursor.is_eof() {
            return Ok(None);
        }
        
        Ok(None)
    }
}
use crate::lexer::Lexer;
use brim_ast::token::TokenKind;
use brim_span::index::ByteIndex;
use brim_span::symbols::Symbol;

pub fn nfc_normalize(string: &str) -> Symbol {
    use unicode_normalization::{IsNormalized, UnicodeNormalization, is_nfc_quick};
    match is_nfc_quick(string.chars()) {
        IsNormalized::Yes => Symbol::new(string),
        _ => {
            let normalized_str: String = string.chars().nfc().collect();
            Symbol::new(&normalized_str)
        }
    }
}

impl Lexer<'_> {
    pub fn ident(&self, start: ByteIndex) -> TokenKind {
        let sym = nfc_normalize(self.content_from(start));
        TokenKind::Ident(sym)
    }
}

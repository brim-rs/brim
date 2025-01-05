use crate::{
    lexer::Lexer,
    parser::{barrel::Barrel, cursor::TokenCursor, symbols::SYMBOL_STRINGS},
};
use anyhow::Result;
use brim::{
    PrimitiveToken, PrimitiveTokenKind,
    compiler::CompilerContext,
    cursor::Cursor,
    files::SimpleFile,
    span::Span,
    symbols::GLOBAL_INTERNER,
    token::{Token, TokenKind},
};
use tracing::debug;
use brim::item::Visibility;
use brim::symbols::Symbol;
use crate::parser::symbols::Pub;

pub mod barrel;
mod cursor;
mod items;
mod symbols;

#[derive(Debug)]
pub struct Parser<'a> {
    pub file: &'a SimpleFile,
    pub cursor: Cursor<'a>,
    pub primitives: Vec<PrimitiveToken>,
    /// Used for documentation generation to keep comments in the AST. Only in `brim doc`.
    pub keep_comments: bool,
    pub tokens: Vec<Token>,
    pub token_cursor: TokenCursor,
    pub current_token: Token,
    pub previous_token: Token,
    pub diags: ParserDiagnostics,
}

#[derive(Debug)]
pub struct ParserDiagnostics {
    pub expected_tokens: Vec<PToken>,
}

impl ParserDiagnostics {
    pub fn new() -> Self {
        Self {
            expected_tokens: vec![]
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PToken {
    pub sym: Symbol,
    pub kind: PTokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PTokenKind {
    Break,
    Catch,
    Const,
    Continue,
    Else,
    Enum,
    False,
    Fn,
    For,
    From,
    If,
    Impl,
    In,
    Let,
    Loop,
    Null,
    Pub,
    Return,
    SelfBig,
    SelfSmall,
    Struct,
    Then,
    Trait,
    True,
    Use,
    While,
}

macro_rules! ptok {
    ($sym:ident) => {
        PToken {
            sym: $sym,
            kind: PTokenKind::$sym,
        }
    };
}

impl<'a> Parser<'a> {
    pub fn new(file: &'a SimpleFile) -> Self {
        let cursor = Cursor::new(file.source());

        Self {
            file,
            cursor,
            primitives: vec![],
            keep_comments: false,
            tokens: vec![],
            token_cursor: TokenCursor::new(vec![]),
            current_token: Token::new(TokenKind::Skipable, Span::DUMMY),
            previous_token: Token::new(TokenKind::Skipable, Span::DUMMY),
            diags: ParserDiagnostics::new(),
        }
    }

    pub fn parse_barrel(&mut self, comp: &mut CompilerContext) -> Result<Barrel> {
        if !GLOBAL_INTERNER.lock().unwrap().initialized {
            GLOBAL_INTERNER.lock().unwrap().initialized = true;

            for (key, value) in SYMBOL_STRINGS.iter() {
                GLOBAL_INTERNER
                    .lock()
                    .unwrap()
                    .add_existing(*key, value.clone());
            }
        }

        debug!("Lexing primitive tokens for a barrel");
        loop {
            let token = self.cursor.next_token();
            self.primitives.push(token.clone());

            if token.kind == PrimitiveTokenKind::Eof {
                break;
            }
        }

        let mut lexer = Lexer::new(self.file, &mut self.primitives);
        let mut tokens = vec![];

        while let Some(token) = lexer.next_token(comp) {
            if token.kind == TokenKind::Eof {
                break;
            }

            if token.kind == TokenKind::Skipable {
                continue;
            }

            tokens.push(token);
        }
        self.tokens = tokens;

        self.token_cursor = TokenCursor::new(self.tokens.clone());
        let mut items = vec![];

        // Advance to get the first token
        self.advance();

        loop {
            let Some(token) = self.parse_item()? else {
                break;
            };

            items.push(token);
        }

        Ok(Barrel {})
    }

    pub fn advance(&mut self) -> Token {
        self.token_cursor.bump();
        self.previous_token = self.current_token.clone();

        if let Some(token) = self.token_cursor.current() {
            self.current_token = token.clone();
        } else {
            self.current_token = Token::new(TokenKind::Eof, Span::DUMMY);
        }

        self.current_token.clone()
    }

    pub fn current(&self) -> Token {
        self.current_token.clone()
    }

    pub fn prev(&self) -> Token {
        self.previous_token.clone()
    }

    pub fn eat_keyword(&mut self, p: PToken) -> bool {
        let is = self.valid_keyword(p);

        if is {
            self.advance();
        }
        is
    }

    pub fn valid_keyword(&mut self, p: PToken) -> bool {
        let is_keyword = self.current().is_keyword(p.sym);

        if !is_keyword {
            self.diags.expected_tokens.push(p)
        }

        is_keyword
    }

    pub fn parse_visibility(&mut self) -> Visibility {
        let is_public = self.eat_keyword(ptok!(Pub));

        if !is_public {
            return Visibility::from_bool(false, self.current().span.from_start());
        }

        Visibility::from_bool(true, self.prev().span)
    }
}

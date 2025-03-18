use crate::{
    lexer::Lexer,
    parser::{cursor::TokenCursor, errors::ExpectedToken},
};
use anyhow::Result;
use brim_ast::{
    ItemId, Match, Pub, SYMBOL_STRINGS,
    item::{FunctionContext, Visibility},
    token::{Delimiter, Orientation, Token, TokenKind},
};
use brim_diagnostics::{ErrorEmitted, box_diag, diagnostic::ToDiagnostic};
use brim_lexer::{PrimitiveToken, PrimitiveTokenKind, cursor::Cursor};
use brim_middle::{
    barrel::Barrel, experimental::Experimental, temp_diag::TemporaryDiagnosticContext,
};
use brim_span::{
    files::get_file,
    span::Span,
    symbols::{GLOBAL_INTERNER, Symbol},
};
use tracing::debug;

mod cursor;
mod errors;
mod expr;
mod generics;
mod items;
mod stmt;
mod ty;

#[derive(Debug)]
pub struct Parser {
    pub file: usize,
    pub primitives: Vec<PrimitiveToken>,
    /// Used for documentation generation to keep comments in the AST. Only in `brim doc`.
    pub keep_comments: bool,
    pub tokens: Vec<Token>,
    pub token_cursor: TokenCursor,
    pub current_token: Token,
    pub previous_token: Token,
    pub last_id: u32,
    pub dcx: TemporaryDiagnosticContext,
    pub experimental: Experimental,
    pub fn_ctx: Option<FunctionContext>,
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
    Comptime,
    Else,
    Enum,
    Extern,
    False,
    Fn,
    For,
    From,
    If,
    In,
    Let,
    Loop,
    Null,
    Match,
    Mod,
    Mut,
    Parent,
    Pub,
    Return,
    SelfBig,
    SelfSmall,
    Struct,
    Then,
    Trait,
    True,
    Type,
    Use,
    While,
}

pub type PResult<T> = Result<T, Box<dyn ToDiagnostic>>;

#[macro_export]
macro_rules! ptok {
    ($sym:ident) => {
        PToken {
            sym: $sym,
            kind: PTokenKind::$sym,
        }
    };
}

#[macro_export]
macro_rules! debug_ident {
    ($( $x:expr ),*) => {
        {
            $(
               #[cfg(debug_assertions)]
                println!("{}", $x.as_ident().unwrap().name);
            )*
        }
    }
}

impl Parser {
    pub fn new(file: usize, experimental: Experimental) -> Self {
        Self {
            file,
            primitives: vec![],
            keep_comments: false,
            tokens: vec![],
            token_cursor: TokenCursor::new(vec![]),
            current_token: Token::new(TokenKind::Skipable, Span::DUMMY),
            previous_token: Token::new(TokenKind::Skipable, Span::DUMMY),
            dcx: TemporaryDiagnosticContext::new(),
            last_id: 0,
            experimental,
            fn_ctx: None,
        }
    }

    pub fn fn_ctx(&self) -> FunctionContext {
        self.fn_ctx.clone().unwrap()
    }

    pub fn set_fn_ctx(&mut self, ctx: FunctionContext) {
        if let None = self.fn_ctx {
            self.fn_ctx = Some(ctx);
        }
    }

    pub fn new_id(&mut self) -> ItemId {
        ItemId::new()
    }

    pub fn parse_barrel(&mut self) -> Result<Barrel> {
        if !GLOBAL_INTERNER.lock().unwrap().initialized {
            GLOBAL_INTERNER.lock().unwrap().initialized = true;

            for (key, value) in SYMBOL_STRINGS.iter() {
                GLOBAL_INTERNER
                    .lock()
                    .unwrap()
                    .add_existing(*key, value.clone());
            }
        }

        let file = get_file(self.file)?;
        debug!("======= Parsing barrel with id: {}", self.file);

        let content = file.source().clone();
        let mut cursor = Cursor::new(&content);

        debug!("Lexing primitive tokens for a barrel");
        loop {
            let token = cursor.next_token();
            self.primitives.push(token.clone());

            if token.kind == PrimitiveTokenKind::Eof {
                break;
            }
        }

        let lex_temp = TemporaryDiagnosticContext::new();
        let mut lexer = Lexer::new(&file, self.primitives.clone(), lex_temp);
        let mut tokens = vec![];

        while let Some(token) = lexer.next_token() {
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
            let token = match self.parse_item() {
                Ok(Some(item)) => item,
                Ok(None) => break,
                Err(diag) => {
                    self.dcx.emit(diag);
                    break;
                }
            };

            items.push(token);
        }

        self.dcx.diags.extend(lexer.ctx.diags);
        debug!("======= Finished parsing barrel with id: {}", self.file);

        Ok(Barrel {
            items,
            id: self.new_id(),
            file_id: self.file,
        })
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

    pub fn is_keyword(&mut self, p: PToken) -> bool {
        self.valid_keyword(p)
    }

    pub fn eat(&mut self, p: TokenKind) -> bool {
        let is = self.current().kind == p;

        if is {
            self.advance();
        }
        is
    }

    pub fn valid_keyword(&mut self, p: PToken) -> bool {
        self.current().is_keyword(p.sym)
    }

    pub fn parse_visibility(&mut self) -> Visibility {
        let is_public = self.eat_keyword(ptok!(Pub));

        if !is_public {
            return Visibility::from_bool(false, self.current().span.from_start());
        }

        Visibility::from_bool(true, self.prev().span)
    }

    pub fn ahead(&self, n: usize) -> Token {
        self.token_cursor.tokens[self.token_cursor.current + n - 1].clone()
    }

    pub fn emit(&mut self, diag: impl ToDiagnostic + 'static) -> ErrorEmitted {
        self.dcx.emit(Box::new(diag));

        ErrorEmitted::new()
    }

    pub fn is_ident(&self) -> bool {
        matches!(self.current().kind, TokenKind::Ident(_)) && !self.current().is_any_keyword()
    }

    pub fn expect(&mut self, p: TokenKind) -> PResult<Token> {
        if self.current().kind == p {
            Ok(self.advance())
        } else {
            box_diag!(ExpectedToken {
                span: (self.current().span, self.file),
                expected: p,
                found: self.current().kind,
            })
        }
    }

    pub fn expect_oparen(&mut self) -> PResult<Token> {
        self.expect(TokenKind::Delimiter(Delimiter::Paren, Orientation::Open))
    }

    pub fn expect_cparen(&mut self) -> PResult<Token> {
        self.expect(TokenKind::Delimiter(Delimiter::Paren, Orientation::Close))
    }

    pub fn expect_obrace(&mut self) -> PResult<Token> {
        self.expect(TokenKind::Delimiter(Delimiter::Brace, Orientation::Open))
    }

    pub fn expect_cbrace(&mut self) -> PResult<Token> {
        self.expect(TokenKind::Delimiter(Delimiter::Brace, Orientation::Close))
    }

    pub fn expect_obracket(&mut self) -> PResult<Token> {
        self.expect(TokenKind::Delimiter(Delimiter::Bracket, Orientation::Open))
    }

    pub fn expect_cbracket(&mut self) -> PResult<Token> {
        self.expect(TokenKind::Delimiter(Delimiter::Bracket, Orientation::Close))
    }

    pub fn eat_while(&mut self, p: TokenKind) {
        while self.eat(p.clone()) {}
    }

    pub fn eat_semis(&mut self) {
        self.eat_while(TokenKind::Semicolon);
    }

    /// Eats until it finds a first brace, returns the span of the brace
    pub fn eat_until_brace(&mut self, orientation: Orientation) -> Span {
        while !self
            .current()
            .is(TokenKind::Delimiter(Delimiter::Brace, orientation))
        {
            self.advance();
        }

        self.prev().span
    }

    pub fn is_paren(&self, o: Orientation) -> bool {
        self.current().is_delimiter(Delimiter::Paren, o)
    }

    pub fn is_brace(&self, o: Orientation) -> bool {
        self.current().is_delimiter(Delimiter::Brace, o)
    }

    pub fn is_bracket(&self, o: Orientation) -> bool {
        self.current().is_delimiter(Delimiter::Bracket, o)
    }

    pub fn eat_possible(&mut self, p: TokenKind) -> bool {
        if self.current().kind == p {
            self.advance();
            true
        } else {
            false
        }
    }

    /// automatically detects if the keyword can be a part of comptime. e.g. match in a type aliast
    pub fn can_begin_comptime(&self) -> bool {
        self.current().is_keyword(Match)
    }

    pub fn save_pos(&self) -> usize {
        self.token_cursor.current
    }

    pub fn restore_pos(&mut self, pos: usize) {
        self.token_cursor.current = pos;
        self.current_token = self
            .tokens
            .get(self.token_cursor.current)
            .unwrap_or(&self.tokens[self.tokens.len() - 1])
            .clone();
        if self.token_cursor.current > 0 {
            self.previous_token = self.tokens[self.token_cursor.current - 1].clone();
        }
    }
}

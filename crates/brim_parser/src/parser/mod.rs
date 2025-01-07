use crate::{
    lexer::Lexer,
    parser::{barrel::Barrel, cursor::TokenCursor},
};
use anyhow::Result;
use brim::{PrimitiveToken, PrimitiveTokenKind, compiler::CompilerContext, cursor::Cursor, files::SimpleFile, span::Span, symbols::GLOBAL_INTERNER, token::{Token, TokenKind}, ErrorEmitted, SYMBOL_STRINGS, Pub, box_diag};
use tracing::debug;
use brim::diag_ctx::{DiagnosticContext, TemporaryDiagnosticContext};
use brim::diagnostic::ToDiagnostic;
use brim::files::get_file;
use brim::item::Visibility;
use brim::symbols::Symbol;
use crate::parser::errors::ExpectedToken;

pub mod barrel;
mod cursor;
mod items;
mod errors;
mod generics;
mod ty;
mod expr;

#[derive(Debug)]
pub struct Parser<'a> {
    pub file: usize,
    pub primitives: Vec<PrimitiveToken>,
    /// Used for documentation generation to keep comments in the AST. Only in `brim doc`.
    pub keep_comments: bool,
    pub tokens: Vec<Token>,
    pub token_cursor: TokenCursor,
    pub current_token: Token,
    pub previous_token: Token,
    pub diags: ParserDiagnostics<'a>,
}

#[derive(Debug)]
pub struct ParserDiagnostics<'a> {
    pub dcx: TemporaryDiagnosticContext<'a>,
}

impl<'a> ParserDiagnostics<'a> {
    pub fn new() -> Self {
        Self {
            dcx: TemporaryDiagnosticContext::new(),
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

pub type PResult<'a, T> = Result<T, Box<dyn ToDiagnostic<'a> + 'a>>;

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

impl<'a> Parser<'a> {
    pub fn new(file: usize) -> Self {
        Self {
            file,
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

        let file = get_file(self.file)?;
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

        let mut lexer = Lexer::new(&file, &mut self.primitives);
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
            let token = match self.parse_item() {
                Ok(Some(item)) => item,
                Ok(None) => break,
                Err(diag) => {
                    self.diags.dcx.emit(diag);
                    break;
                }
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

    pub fn emit(&mut self, diag: impl ToDiagnostic<'a> + 'a) -> ErrorEmitted {
        self.diags.dcx.emit(Box::new(diag));

        ErrorEmitted::new()
    }

    pub fn is_ident(&self) -> bool {
        matches!(self.current().kind, TokenKind::Ident(_)) && !self.current().is_any_keyword()
    }

    pub fn expect(&mut self, p: TokenKind) -> PResult<'a, Token> {
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
}

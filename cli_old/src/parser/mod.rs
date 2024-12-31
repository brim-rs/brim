use crate::lexer::tokens::{Token, TokenKind};

mod expressions;
mod statements;

use crate::{ast::Ast, error::parser_error};
use anyhow::{bail, Result};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseContext {
    Normal,
    IfCondition,
    WhileCondition,
}

#[derive(Debug, Clone)]
pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub context_stack: Vec<ParseContext>,
    pub ast: Ast,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, ast: Ast) -> Self {
        Self {
            tokens,
            current: 0,
            context_stack: vec![ParseContext::Normal],
            ast,
        }
    }

    pub fn current_context(&self) -> &ParseContext {
        self.context_stack
            .last()
            .expect("Parser context stack should never be empty")
    }

    pub fn push_context(&mut self, context: ParseContext) {
        self.context_stack.push(context);
    }

    pub fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    pub fn is_context(&self, context: &ParseContext) -> bool {
        self.current_context() == context
    }
}

impl Parser {
    pub fn parse(&mut self) -> Result<()> {
        while !self.is_eof() {
            if let Some(stmt_id) = self.parse_stmt()? {
                self.ast.new_item(stmt_id);

                self.expect_punct(TokenKind::Semicolon)?;
            }
        }

        Ok(())
    }

    pub fn consume(&mut self) -> Token {
        if !self.is_eof() {
            self.current += 1;
        }

        self.previous()
    }

    pub fn previous(&self) -> Token {
        assert!(self.current > 0);
        self.tokens[self.current - 1].clone()
    }

    pub fn peek(&self) -> Token {
        self.tokens.get(self.current).cloned().unwrap_or_else(|| {
            Token::new(
                TokenKind::EOF,
                self.tokens.get(self.tokens.len() - 1).unwrap().span.clone(),
            )
        })
    }

    pub fn peek_next(&self) -> Token {
        self.tokens[self.current + 1].clone()
    }

    pub fn is_eof(&self) -> bool {
        self.current >= self.tokens.len() || self.peek().kind == TokenKind::EOF
    }

    pub fn possible_check(&mut self, kind: TokenKind) {
        if self.peek().kind == kind {
            self.consume();
        }
    }

    pub fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.peek();

        if token.kind == kind {
            Ok(self.consume())
        } else {
            bail!(parser_error(
                format!("Expected token of kind: {}", kind),
                vec![(token.span.clone(), None)],
                vec![],
                None
            ));
        }
    }

    pub fn expect_punct(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.peek();

        if token.kind == kind {
            Ok(self.consume())
        } else {
            let prev = self.previous();
            let len = kind.to_string().len();

            bail!(parser_error(
                format!("Expected token of kind: {}", kind),
                vec![(
                    prev.span
                        .move_right(len + prev.span.literal.len())
                        .shorten(len),
                    "This is where the expected token should be"
                        .to_string()
                        .into()
                )],
                vec![],
                None
            ));
        }
    }
}

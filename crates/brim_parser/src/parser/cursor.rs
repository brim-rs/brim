use brim::token::Token;

#[derive(Debug)]
pub struct TokenCursor {
    pub tokens: Vec<Token>,
    pub current: usize,
}

impl TokenCursor {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn current(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    pub fn bump(&mut self) {
        self.current += 1;
    }

    pub fn is_eof(&self) -> bool {
        self.current >= self.tokens.len()
    }
}

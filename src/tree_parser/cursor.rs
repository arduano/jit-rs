use crate::macro_builder::{JitToken, JitTokenTree};

pub struct ParseCursor<'a> {
    tokens: &'a JitTokenTree,
    index: usize,
}

impl<'a> ParseCursor<'a> {
    pub fn new(tokens: &'a JitTokenTree) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn next(&mut self) -> Option<&'a JitToken> {
        let token = self.tokens.tokens.get(self.index);
        self.index += 1;
        token
    }

    pub fn peek(&self, ahead: usize) -> Option<&'a JitToken> {
        self.tokens.tokens.get(self.index + ahead)
    }

    pub fn is_empty(&self) -> bool {
        self.index >= self.tokens.tokens.len()
    }

    pub fn fork(&self) -> Self {
        Self {
            tokens: self.tokens,
            index: self.index,
        }
    }

    pub fn reset(&mut self, cursor: &Self) {
        self.index = cursor.index;
    }
}

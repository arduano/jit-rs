use std::borrow::Cow;

use crate::macro_builder::{JitBasicToken, JitGroupKind, JitToken, JitTokenKind, JitTokenTree};

#[derive(Debug, Clone)]
pub enum ParseResult<'a, T> {
    Ok(ParseCursor<'a>, T),
    NoMatch { expected: &'static str },
    Error(ParseError),
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: Cow<'static, str>,
}

impl ParseError {
    pub fn new(message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl<'a, T> ParseResult<'a, T> {
    pub fn no_match_into_error(self) -> ParseResult<'a, T> {
        match self {
            ParseResult::NoMatch { expected } => ParseResult::Error(ParseError {
                message: format!("expected {}", expected).into(),
            }),
            _ => self,
        }
    }

    pub fn no_match(expected: &'static str) -> ParseResult<'a, T> {
        ParseResult::NoMatch { expected }
    }

    pub fn error(message: impl Into<Cow<'static, str>>) -> ParseResult<'a, T> {
        ParseResult::Error(ParseError {
            message: message.into(),
        })
    }

    pub fn into_result(self) -> Result<T, ParseError> {
        match self {
            ParseResult::Ok(_, t) => Ok(t),
            ParseResult::NoMatch { expected } => Err(ParseError {
                message: format!("expected {}", expected).into(),
            }),
            ParseResult::Error(e) => Err(e),
        }
    }

    pub fn is_ok(&self) -> bool {
        matches!(self, ParseResult::Ok(..))
    }

    pub fn skip_if(self, cond: bool) -> Self {
        if cond {
            return ParseResult::NoMatch {
                expected: "skip_if",
            };
        } else {
            self
        }
    }
}

#[derive(Debug, Clone)]
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

    pub fn parse_next_basic(&mut self, basic: JitBasicToken) -> bool {
        if self.peek_next_basic(basic) {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn peek_next_basic(&mut self, basic: JitBasicToken) -> bool {
        if let Some(JitToken {
            kind: JitTokenKind::Basic(b),
            ..
        }) = self.peek(0)
        {
            if *b == basic {
                return true;
            }
        }

        false
    }

    pub fn parse_next_group(
        &mut self,
        group_kind: JitGroupKind,
    ) -> Result<ParseCursor<'a>, ParseError> {
        if let Some(JitToken {
            kind: JitTokenKind::Grouped { kind, tree },
            ..
        }) = self.peek(0)
        {
            if *kind == group_kind {
                self.next();
                return Ok(ParseCursor::new(tree));
            }
        }

        Err(ParseError::new(format!("expected {}", group_kind)))
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

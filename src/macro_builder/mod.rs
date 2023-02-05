use std::borrow::Cow;

#[derive(Debug, Clone)]
pub struct JitSpan {}

#[derive(Debug, Clone, Copy)]
pub enum JitTokenIntegerBits {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
}

#[derive(Debug, Clone, Copy)]
pub enum JitTokenFloatBits {
    Bits32,
    Bits64,
}

#[derive(Debug, Clone, Copy)]
pub enum JitTokenNumberKind {
    SignedInt(JitTokenIntegerBits),
    UnsignedInt(JitTokenIntegerBits),
    Float(JitTokenFloatBits),
}

#[derive(Debug, Clone)]
pub enum JitTokenKind {
    Pub,
    Fn,
    If,
    While,
    Else,
    Ident(Cow<'static, str>),
    Number(JitTokenNumberKind, Cow<'static, str>),
    Bool(bool),
    Colon,
    Comma,
    Arrow,
    Impl,
    Dot,
    Braced(JitTokenTree),
    Bracketed(JitTokenTree),
    Parenthesized(JitTokenTree),
    Semicolon,
    Return,
    Let,
    Equal,
    Pipe,
    And,
    Caret,
    Loop,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    LeftAngBracket,
    RightAngBracket,
}

#[derive(Debug, Clone)]
pub struct JitToken {
    pub kind: JitTokenKind,
    pub span: JitSpan,
}

impl JitToken {
    pub fn new(kind: JitTokenKind, span: JitSpan) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone)]
pub struct JitTokenTree {
    pub tokens: Vec<JitToken>,
}

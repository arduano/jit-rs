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

impl std::fmt::Display for JitTokenNumberKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            JitTokenNumberKind::SignedInt(bits) => match bits {
                JitTokenIntegerBits::Bits8 => write!(f, "i8"),
                JitTokenIntegerBits::Bits16 => write!(f, "i16"),
                JitTokenIntegerBits::Bits32 => write!(f, "i32"),
                JitTokenIntegerBits::Bits64 => write!(f, "i64"),
            },
            JitTokenNumberKind::UnsignedInt(bits) => match bits {
                JitTokenIntegerBits::Bits8 => write!(f, "u8"),
                JitTokenIntegerBits::Bits16 => write!(f, "u16"),
                JitTokenIntegerBits::Bits32 => write!(f, "u32"),
                JitTokenIntegerBits::Bits64 => write!(f, "u64"),
            },
            JitTokenNumberKind::Float(bits) => match bits {
                JitTokenFloatBits::Bits32 => write!(f, "f32"),
                JitTokenFloatBits::Bits64 => write!(f, "f64"),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum JitTokenKind {
    Ident(Cow<'static, str>),
    Number(JitTokenNumberKind, Cow<'static, str>),
    Bool(bool),
    Grouped {
        kind: JitGroupKind,
        tree: JitTokenTree,
    },
    Basic(JitBasicToken),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum JitBasicToken {
    Semicolon,
    Return,
    Let,
    DoubleEqual,
    Equal,
    Pipe,
    Ampersand,
    Caret,
    Loop,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    LeftAngBracket,
    RightAngBracket,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Pub,
    Fn,
    If,
    While,
    Else,
    Colon,
    Comma,
    Arrow,
    Impl,
    Dot,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum JitGroupKind {
    Braces,
    Brackets,
    Parentheses,
}

impl std::fmt::Display for JitGroupKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JitGroupKind::Braces => write!(f, "{{}}"),
            JitGroupKind::Brackets => write!(f, "[]"),
            JitGroupKind::Parentheses => write!(f, "()"),
        }
    }
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

impl std::fmt::Display for JitTokenTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let printer = JitTokenTreePrinter {
            tokens: &self.tokens,
            indent: 0,
            start_newline: false,
        };
        write!(f, "{}", printer)
    }
}

struct JitTokenTreePrinter<'a> {
    tokens: &'a [JitToken],
    indent: usize,
    start_newline: bool,
}

impl std::fmt::Display for JitTokenTreePrinter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut newline = self.start_newline;
        for token in self.tokens {
            if newline {
                for _ in 0..self.indent {
                    write!(f, "    ")?;
                }
                newline = false;
            }

            match &token.kind {
                JitTokenKind::Grouped { kind, tree } => match kind {
                    JitGroupKind::Braces => {
                        writeln!(f, "{{")?;
                        let inner_printer = JitTokenTreePrinter {
                            tokens: &tree.tokens,
                            indent: self.indent + 1,
                            start_newline: true,
                        };
                        write!(f, "{}", inner_printer)?;
                        for _ in 0..self.indent {
                            write!(f, "    ")?;
                        }
                        writeln!(f, "}}")?;
                        newline = true;
                    }
                    JitGroupKind::Brackets => {
                        write!(f, "[ ")?;
                        let inner_printer = JitTokenTreePrinter {
                            tokens: &tree.tokens,
                            indent: self.indent + 1,
                            start_newline: false,
                        };
                        write!(f, "{}", inner_printer)?;
                        write!(f, "]")?;
                    }
                    JitGroupKind::Parentheses => {
                        write!(f, "( ")?;
                        let inner_printer = JitTokenTreePrinter {
                            tokens: &tree.tokens,
                            indent: self.indent + 1,
                            start_newline: false,
                        };
                        write!(f, "{}", inner_printer)?;
                        write!(f, ")")?;
                    }
                },

                JitTokenKind::Ident(ident) => write!(f, "{}", ident)?,
                JitTokenKind::Number(size, val) => write!(f, "{}{}", val, size)?,
                JitTokenKind::Bool(val) => write!(f, "{}", val)?,

                JitTokenKind::Basic(basic) => match basic {
                    JitBasicToken::Semicolon => {
                        write!(f, ";")?;
                        newline = true;
                    }

                    JitBasicToken::Pub => write!(f, "pub")?,
                    JitBasicToken::Fn => write!(f, "fn")?,
                    JitBasicToken::If => write!(f, "if")?,
                    JitBasicToken::While => write!(f, "while")?,
                    JitBasicToken::Else => write!(f, "else")?,
                    JitBasicToken::Colon => write!(f, ":")?,
                    JitBasicToken::Comma => write!(f, ",")?,
                    JitBasicToken::Arrow => write!(f, "->")?,
                    JitBasicToken::Impl => write!(f, "impl")?,
                    JitBasicToken::Dot => write!(f, ".")?,
                    JitBasicToken::Return => write!(f, "return")?,
                    JitBasicToken::Let => write!(f, "let")?,
                    JitBasicToken::Equal => write!(f, "=")?,
                    JitBasicToken::Pipe => write!(f, "|")?,
                    JitBasicToken::Ampersand => write!(f, "&")?,
                    JitBasicToken::Caret => write!(f, "^")?,
                    JitBasicToken::Loop => write!(f, "loop")?,
                    JitBasicToken::Plus => write!(f, "+")?,
                    JitBasicToken::Star => write!(f, "*")?,
                    JitBasicToken::Slash => write!(f, "/")?,
                    JitBasicToken::Percent => write!(f, "%")?,
                    JitBasicToken::LeftAngBracket => write!(f, "<")?,
                    JitBasicToken::RightAngBracket => write!(f, ">")?,
                    JitBasicToken::Minus => write!(f, "-")?,
                    JitBasicToken::NotEqual => write!(f, "!=")?,
                    JitBasicToken::LessEqual => write!(f, "<=")?,
                    JitBasicToken::GreaterEqual => write!(f, ">=")?,
                    JitBasicToken::DoubleEqual => write!(f, "==")?,
                },
            }

            if newline {
                writeln!(f)?;
            } else {
                write!(f, " ")?;
            }
        }

        Ok(())
    }
}

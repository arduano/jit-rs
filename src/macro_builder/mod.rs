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
                JitTokenKind::Braced(tokens) => {
                    writeln!(f, "{{")?;
                    let inner_printer = JitTokenTreePrinter {
                        tokens: &tokens.tokens,
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
                JitTokenKind::Bracketed(tokens) => {
                    write!(f, "[ ")?;
                    let inner_printer = JitTokenTreePrinter {
                        tokens: &tokens.tokens,
                        indent: self.indent + 1,
                        start_newline: false,
                    };
                    write!(f, "{}", inner_printer)?;
                    write!(f, "]")?;
                }
                JitTokenKind::Parenthesized(tokens) => {
                    write!(f, "( ")?;
                    let inner_printer = JitTokenTreePrinter {
                        tokens: &tokens.tokens,
                        indent: self.indent + 1,
                        start_newline: false,
                    };
                    write!(f, "{}", inner_printer)?;
                    write!(f, ")")?;
                }

                JitTokenKind::Semicolon => {
                    write!(f, ";")?;
                    newline = true;
                }

                JitTokenKind::Pub => write!(f, "pub")?,
                JitTokenKind::Fn => write!(f, "fn")?,
                JitTokenKind::If => write!(f, "if")?,
                JitTokenKind::While => write!(f, "while")?,
                JitTokenKind::Else => write!(f, "else")?,
                JitTokenKind::Ident(ident) => write!(f, "{}", ident)?,
                JitTokenKind::Number(size, val) => write!(f, "{}{}", val, size)?,
                JitTokenKind::Bool(val) => write!(f, "{}", val)?,
                JitTokenKind::Colon => write!(f, ":")?,
                JitTokenKind::Comma => write!(f, ",")?,
                JitTokenKind::Arrow => write!(f, "->")?,
                JitTokenKind::Impl => write!(f, "impl")?,
                JitTokenKind::Dot => write!(f, ".")?,
                JitTokenKind::Return => write!(f, "return")?,
                JitTokenKind::Let => write!(f, "let")?,
                JitTokenKind::Equal => write!(f, "=")?,
                JitTokenKind::Pipe => write!(f, "|")?,
                JitTokenKind::And => write!(f, "&")?,
                JitTokenKind::Caret => write!(f, "^")?,
                JitTokenKind::Loop => write!(f, "loop")?,
                JitTokenKind::Plus => write!(f, "+")?,
                JitTokenKind::Star => write!(f, "*")?,
                JitTokenKind::Slash => write!(f, "/")?,
                JitTokenKind::Percent => write!(f, "%")?,
                JitTokenKind::LeftAngBracket => write!(f, "<")?,
                JitTokenKind::RightAngBracket => write!(f, ">")?,
                JitTokenKind::Minus => write!(f, "-")?,
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

use std::borrow::Cow;

pub use crate::common::{FloatBits, IntBits, NumberKind, NumberValue};

#[derive(Debug, Clone)]
pub struct JitSpan {}

#[derive(Debug, Clone)]
pub enum JitTokenKind {
    Ident(Cow<'static, str>),
    Number(NumberValue),
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
    Const,
    Let,
    DoubleEqual,
    Equal,
    Pipe,
    Ampersand,
    Caret,
    Loop,
    Break,
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
    Struct,
    If,
    While,
    Else,
    Colon,
    Comma,
    Arrow,
    Impl,
    Dot,
    As,
    DoubleColon,
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
            is_inside_square_brackets: false,
        };
        write!(f, "{}", printer)
    }
}

struct JitTokenTreePrinter<'a> {
    tokens: &'a [JitToken],
    indent: usize,
    start_newline: bool,
    is_inside_square_brackets: bool,
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
                            is_inside_square_brackets: false,
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
                            is_inside_square_brackets: true,
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
                            is_inside_square_brackets: false,
                        };
                        write!(f, "{}", inner_printer)?;
                        write!(f, ")")?;
                    }
                },

                JitTokenKind::Ident(ident) => write!(f, "{}", ident)?,
                JitTokenKind::Number(val) => write!(f, "{}{}", val, val.kind())?,
                JitTokenKind::Bool(val) => write!(f, "{}", val)?,

                JitTokenKind::Basic(basic) => match basic {
                    JitBasicToken::Semicolon => {
                        write!(f, ";")?;
                        if !self.is_inside_square_brackets {
                            newline = true;
                        }
                    }

                    JitBasicToken::Pub => write!(f, "pub")?,
                    JitBasicToken::Fn => write!(f, "fn")?,
                    JitBasicToken::Struct => write!(f, "struct")?,
                    JitBasicToken::Const => write!(f, "const")?,
                    JitBasicToken::If => write!(f, "if")?,
                    JitBasicToken::While => write!(f, "while")?,
                    JitBasicToken::Else => write!(f, "else")?,
                    JitBasicToken::Break => write!(f, "break")?,
                    JitBasicToken::As => write!(f, "as")?,
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
                    JitBasicToken::DoubleColon => write!(f, "::")?,
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

pub trait JitTokenWritable {
    fn len(&self) -> usize;
    fn write(self, tokens: &mut Vec<JitToken>, span: JitSpan);
}

impl JitTokenWritable for JitTokenTree {
    fn len(&self) -> usize {
        self.tokens.len()
    }

    fn write(mut self, tokens: &mut Vec<JitToken>, _: JitSpan) {
        tokens.append(&mut self.tokens)
    }
}

impl JitTokenWritable for &JitTokenTree {
    fn len(&self) -> usize {
        self.tokens.len()
    }

    fn write(self, tokens: &mut Vec<JitToken>, _: JitSpan) {
        tokens.reserve(self.tokens.len());
        tokens.extend(self.tokens.iter().cloned())
    }
}

impl JitTokenWritable for JitTokenKind {
    fn len(&self) -> usize {
        1
    }

    fn write(self, tokens: &mut Vec<JitToken>, span: JitSpan) {
        tokens.push(JitToken { kind: self, span })
    }
}

macro_rules! impl_writable_num {
    ($num:ty, $var:ident) => {
        impl JitTokenWritable for $num {
            fn len(&self) -> usize {
                1
            }

            fn write(self, tokens: &mut Vec<JitToken>, span: JitSpan) {
                tokens.push(JitToken {
                    kind: JitTokenKind::Number(NumberValue::$var(self)),
                    span,
                })
            }
        }
    };
}

impl_writable_num!(u8, U8);
impl_writable_num!(u16, U16);
impl_writable_num!(u32, U32);
impl_writable_num!(u64, U64);
impl_writable_num!(usize, USize);
impl_writable_num!(i8, I8);
impl_writable_num!(i16, I16);
impl_writable_num!(i32, I32);
impl_writable_num!(i64, I64);
impl_writable_num!(isize, ISize);
impl_writable_num!(f32, F32);
impl_writable_num!(f64, F64);

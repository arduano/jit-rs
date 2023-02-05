use crate::{
    macro_builder::{JitBasicToken, JitToken, JitTokenKind},
    tree_parser::parser::{ParseCursor, ParseResult, TreeParseItem},
};

#[derive(Debug, Clone, Copy)]
pub enum TreeBinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Gt,
    Eq,
    Neq,
    Lte,
    Gte,
    BinaryAnd,
    BinaryOr,
    BinaryXor,
}

impl TreeBinaryOpKind {
    const KIND: &'static str = "binary operator";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let next = cursor.peek(0);

        let op = match next {
            Some(JitToken { kind, span }) => match kind {
                JitTokenKind::Basic(basic) => match basic {
                    JitBasicToken::Plus => Ok(Self::Add),
                    JitBasicToken::Minus => Ok(Self::Sub),
                    JitBasicToken::Star => Ok(Self::Mul),
                    JitBasicToken::Slash => Ok(Self::Div),
                    JitBasicToken::Percent => Ok(Self::Mod),
                    JitBasicToken::LeftAngBracket => Ok(Self::Lt),
                    JitBasicToken::RightAngBracket => Ok(Self::Gt),
                    JitBasicToken::DoubleEqual => Ok(Self::Eq),
                    JitBasicToken::NotEqual => Ok(Self::Neq),
                    JitBasicToken::LessEqual => Ok(Self::Lte),
                    JitBasicToken::GreaterEqual => Ok(Self::Gte),
                    JitBasicToken::Ampersand => Ok(Self::BinaryAnd),
                    JitBasicToken::Pipe => Ok(Self::BinaryOr),
                    JitBasicToken::Caret => Ok(Self::BinaryXor),
                    _ => Err(ParseResult::no_match(Self::KIND)),
                },
                _ => Err(ParseResult::no_match(Self::KIND)),
            },
            None => Err(ParseResult::no_match(Self::KIND)),
        };

        match op {
            Ok(op) => {
                cursor.next();
                ParseResult::Ok(cursor, op)
            }
            Err(err) => err,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TreeUnaryOpKind {
    Neg,
}

impl TreeUnaryOpKind {
    const KIND: &'static str = "unary operator";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let next = cursor.peek(0);

        let op = match next {
            Some(JitToken { kind, span }) => match kind {
                JitTokenKind::Basic(basic) => match basic {
                    JitBasicToken::Minus => Ok(Self::Neg),
                    _ => Err(ParseResult::no_match(Self::KIND)),
                },
                _ => Err(ParseResult::no_match(Self::KIND)),
            },
            None => Err(ParseResult::no_match(Self::KIND)),
        };

        match op {
            Ok(op) => {
                cursor.next();
                ParseResult::Ok(cursor, op)
            }
            Err(err) => err,
        }
    }
}

use std::borrow::Cow;

use crate::{
    macro_builder::{JitToken, JitTokenKind, JitTokenNumberKind},
    tree_parser::parser::{ParseCursor, ParseResult, TreeParseItem},
};

#[derive(Debug, Clone)]
pub struct TreeNumberLiteral {
    pub value: Cow<'static, str>,
    pub ty: JitTokenNumberKind,
}

impl TreeNumberLiteral {
    const KIND: &'static str = "number literal";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let token = cursor.next();

        if let Some(JitToken {
            kind: JitTokenKind::Number(ty, str),
            span: _,
        }) = token
        {
            ParseResult::Ok(
                cursor,
                Self {
                    value: str.clone(),
                    ty: ty.clone(),
                },
            )
        } else {
            ParseResult::no_match(Self::KIND)
        }
    }
}

#[derive(Debug, Clone)]
pub struct TreeBoolLiteral {
    pub value: bool,
}

impl TreeBoolLiteral {
    const KIND: &'static str = "bool literal";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let token = cursor.next();

        if let Some(JitToken {
            kind: JitTokenKind::Bool(val),
            span: _,
        }) = token
        {
            ParseResult::Ok(cursor, Self { value: *val })
        } else {
            ParseResult::no_match(Self::KIND)
        }
    }
}

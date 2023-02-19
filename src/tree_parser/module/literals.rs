use std::borrow::Cow;

use crate::{
    common::NumberKind,
    macro_builder::{JitToken, JitTokenKind},
    tree_parser::{
        macros::pass_val,
        parser::{ParseCursor, ParseResult},
    },
};

#[derive(Debug, Clone)]
pub struct TreeNumberLiteral {
    pub value: Cow<'static, str>,
    pub ty: NumberKind,
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

#[derive(Debug, Clone)]
pub enum TreeLiteral {
    Number(TreeNumberLiteral),
    Bool(TreeBoolLiteral),
}

impl TreeLiteral {
    const KIND: &'static str = "literal";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if let Some(lit) = pass_val!(cursor, TreeNumberLiteral::parse(cursor.clone())) {
            return ParseResult::Ok(cursor, Self::Number(lit));
        } else if let Some(lit) = pass_val!(cursor, TreeBoolLiteral::parse(cursor.clone())) {
            return ParseResult::Ok(cursor, Self::Bool(lit));
        } else {
            return ParseResult::no_match(Self::KIND);
        }
    }
}

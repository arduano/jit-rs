use std::borrow::Cow;

use crate::{
    macro_builder::{JitBasicToken, JitToken, JitTokenKind},
    tree_parser::{
        macros::{get_required_val, ident_or_error},
        parser::{ParseCursor, ParseResult},
    },
};

use super::TreeLiteral;

#[derive(Debug, Clone)]
pub struct TreeConstant {
    pub name: Cow<'static, str>,
    pub value: TreeLiteral,
}

impl TreeConstant {
    const KIND: &'static str = "constant";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if !cursor.parse_next_basic(JitBasicToken::Const) {
            return ParseResult::no_match(Self::KIND);
        }

        let name = ident_or_error!(cursor, "expected const name");

        if !cursor.parse_next_basic(JitBasicToken::Equal) {
            return ParseResult::error("expected '='");
        }

        let value = get_required_val!(cursor, TreeLiteral::parse(cursor));

        if !cursor.parse_next_basic(JitBasicToken::Semicolon) {
            return ParseResult::error("expected ';'");
        }

        ParseResult::Ok(
            cursor,
            Self {
                name: name.clone(),
                value,
            },
        )
    }
}

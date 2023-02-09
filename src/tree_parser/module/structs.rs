use std::borrow::Cow;

use crate::{
    macro_builder::{JitBasicToken, JitGroupKind, JitToken, JitTokenKind},
    tree_parser::{
        macros::{get_required_val, ident_or_error, pass_val},
        parser::{ParseCursor, ParseResult},
    },
};

use super::TreeType;

#[derive(Debug, Clone)]
pub struct TreeStruct {
    pub name: Cow<'static, str>,
    pub fields: Vec<TreeStructField>,
}

impl TreeStruct {
    const KIND: &'static str = "function";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if !cursor.parse_next_basic(JitBasicToken::Struct) {
            return ParseResult::no_match(Self::KIND);
        }

        let name = ident_or_error!(cursor, "expected struct name");
        if name.chars().nth(0).unwrap().is_lowercase() {
            return ParseResult::error("struct names must start with an uppercase letter");
        }

        let mut fields_cursor = pass_val!(cursor.parse_next_group(JitGroupKind::Braces));
        let mut fields = Vec::new();

        while !fields_cursor.is_empty() {
            let field = get_required_val!(fields_cursor, TreeStructField::parse(fields_cursor));
            fields.push(field);

            let next_comma = fields_cursor.parse_next_basic(JitBasicToken::Comma);
            if !next_comma && !fields_cursor.is_empty() {
                return ParseResult::error("expected comma");
            }
        }

        ParseResult::Ok(
            cursor,
            Self {
                name: name.clone(),
                fields,
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct TreeStructField {
    pub name: Cow<'static, str>,
    pub ty: TreeType,
}

impl TreeStructField {
    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let name = ident_or_error!(cursor, "struct field");

        if !cursor.parse_next_basic(JitBasicToken::Colon) {
            return ParseResult::no_match("struct field");
        }

        let ty = get_required_val!(cursor, TreeType::parse(cursor));

        ParseResult::Ok(
            cursor,
            Self {
                name: name.clone(),
                ty,
            },
        )
    }
}

use std::borrow::Cow;

use crate::{
    macro_builder::{
        JitBasicToken, JitGroupKind, JitToken, JitTokenIntegerBits, JitTokenKind,
        JitTokenNumberKind,
    },
    tree_parser::{
        macros::{get_required_val, pass_val},
        parser::{ParseCursor, ParseResult},
    },
};

use super::TreeNumberLiteral;

#[derive(Debug, Clone)]
pub enum TreeType {
    Base(BaseTreeType),
    ConstArray(Box<TreeType>, u32),
    Ptr(Box<TreeType>),
}

#[derive(Debug, Clone)]
pub struct BaseTreeType {
    pub name: Cow<'static, str>,
}

impl TreeType {
    const KIND: &'static str = "type";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let is_ptr = cursor.parse_next_basic(JitBasicToken::Star);
        if is_ptr {
            let res = TreeType::parse(cursor).no_match_into_error();
            if let ParseResult::Ok(cursor, ty) = res {
                return ParseResult::Ok(cursor, Self::Ptr(Box::new(ty)));
            } else {
                return res;
            }
        }

        if let Some(ty) = pass_val!(cursor, Self::parse_basic_type(cursor.clone())) {
            return ParseResult::Ok(cursor, ty);
        } else if let Some(ty) = pass_val!(cursor, Self::parse_const_array_type(cursor.clone())) {
            return ParseResult::Ok(cursor, ty);
        } else {
            return ParseResult::no_match(Self::KIND);
        }
    }

    pub fn parse_const_array_type<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, TreeType> {
        if let Ok(mut inner_cursor) = cursor.parse_next_group(JitGroupKind::Brackets) {
            let inner_ty =
                get_required_val!(inner_cursor, Self::parse_basic_type(inner_cursor.clone()));

            if !inner_cursor.parse_next_basic(JitBasicToken::Semicolon) {
                return ParseResult::error("expected semicolon");
            }

            let size =
                get_required_val!(inner_cursor, TreeNumberLiteral::parse(inner_cursor.clone()));
            if size.ty != JitTokenNumberKind::UnsignedInt(JitTokenIntegerBits::Bits32) {
                return ParseResult::error("expected 32-bit unsigned integer");
            }

            let Ok(size) = size.value.parse() else {
                return ParseResult::error("expected number");
            };

            if !inner_cursor.is_empty() {
                return ParseResult::error("expected end of brackets");
            }

            ParseResult::Ok(
                cursor,
                // FIXME: Unwrap error handling
                Self::ConstArray(Box::new(inner_ty), size),
            )
        } else {
            ParseResult::no_match("const array type")
        }
    }

    pub fn parse_basic_type<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, TreeType> {
        let token = cursor.next();

        if let Some(JitToken {
            kind: JitTokenKind::Ident(name),
            span: _,
        }) = token
        {
            ParseResult::Ok(cursor, Self::Base(BaseTreeType { name: name.clone() }))
        } else {
            ParseResult::no_match(Self::KIND)
        }
    }
}

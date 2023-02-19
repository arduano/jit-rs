use std::borrow::Cow;

use crate::{
    common::{IntBits, NumberKind},
    macro_builder::{JitBasicToken, JitGroupKind, JitToken, JitTokenKind},
    tree_parser::{
        macros::{get_required_val, pass_val},
        parser::{ParseCursor, ParseResult},
    },
};

use super::{TreeBoolLiteral, TreeNumberLiteral};

#[derive(Debug, Clone)]
pub enum TreeType {
    Base(BaseTreeType),
    ConstArray(Box<TreeType>, u32),
    Vector(Box<TreeType>, u32),
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
        } else if let Some(ty) = pass_val!(cursor, Self::parse_array_type(cursor.clone())) {
            return ParseResult::Ok(cursor, ty);
        } else if let Some(ty) = pass_val!(cursor, Self::parse_vector_type(cursor.clone())) {
            return ParseResult::Ok(cursor, ty);
        } else {
            return ParseResult::no_match(Self::KIND);
        }
    }

    pub fn parse_array_type<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, TreeType> {
        if let Ok(mut inner_cursor) = cursor.parse_next_group(JitGroupKind::Brackets) {
            let inner_ty =
                get_required_val!(inner_cursor, Self::parse_basic_type(inner_cursor.clone()));

            if !inner_cursor.parse_next_basic(JitBasicToken::Semicolon) {
                return ParseResult::error("expected semicolon");
            }

            let size =
                get_required_val!(inner_cursor, TreeNumberLiteral::parse(inner_cursor.clone()));
            if size.ty != NumberKind::UnsignedInt(IntBits::BitsSize) {
                return ParseResult::error("expected usize integer");
            }

            let Ok(size) = size.value.parse() else {
                return ParseResult::error("expected number");
            };

            if !inner_cursor.is_empty() {
                return ParseResult::error("expected end of brackets");
            }

            ParseResult::Ok(cursor, Self::ConstArray(Box::new(inner_ty), size))
        } else {
            ParseResult::no_match("const array type")
        }
    }

    pub fn parse_vector_type<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, TreeType> {
        if cursor.parse_next_basic(JitBasicToken::LeftAngBracket) {
            let inner_ty = get_required_val!(cursor, Self::parse_basic_type(cursor.clone()));

            if !cursor.parse_next_basic(JitBasicToken::Semicolon) {
                return ParseResult::error("expected semicolon");
            }

            let size = get_required_val!(cursor, TreeNumberLiteral::parse(cursor.clone()));

            if size.ty != NumberKind::UnsignedInt(IntBits::BitsSize) {
                return ParseResult::error("expected usize integer");
            }

            let Ok(size) = size.value.parse() else {
                return ParseResult::error("expected number");
            };

            if !cursor.parse_next_basic(JitBasicToken::RightAngBracket) {
                return ParseResult::error("expected end of brackets");
            }

            ParseResult::Ok(cursor, Self::Vector(Box::new(inner_ty), size))
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

#[derive(Debug, Clone)]
pub enum TreeTypeMarker {
    Type(TreeType),
    NumLiteral(TreeNumberLiteral),
    BoolLiteral(TreeBoolLiteral),
}

impl TreeTypeMarker {
    const KIND: &'static str = "type marker";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if let Some(val) = pass_val!(cursor, TreeType::parse(cursor.clone())) {
            return ParseResult::Ok(cursor, Self::Type(val));
        } else if let Some(val) = pass_val!(cursor, TreeNumberLiteral::parse(cursor.clone())) {
            return ParseResult::Ok(cursor, Self::NumLiteral(val));
        } else if let Some(val) = pass_val!(cursor, TreeBoolLiteral::parse(cursor.clone())) {
            return ParseResult::Ok(cursor, Self::BoolLiteral(val));
        } else {
            return ParseResult::no_match(Self::KIND);
        }
    }
}

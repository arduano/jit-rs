use std::borrow::Cow;

use crate::{
    macro_builder::{JitBasicToken, JitGroupKind, JitToken, JitTokenKind},
    tree_parser::{
        macros::{get_required_val, ident_or_error, pass_val},
        parser::{ParseCursor, ParseResult},
    },
};

use super::{TreeBody, TreeType, TreeTypeMarker};

#[derive(Debug, Clone)]
pub struct TreeFunctionNameWithMarkers {
    pub name: Cow<'static, str>,
    pub ty_markers: Vec<TreeTypeMarker>,
}

#[derive(Debug, Clone)]
pub struct TreeFunction {
    pub public: bool,
    pub name: TreeFunctionNameWithMarkers,
    pub args: Vec<TreeFunctionArg>,
    pub ret_type: Option<TreeType>,
    pub body: TreeBody,
}

impl TreeFunction {
    const KIND: &'static str = "function";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let is_pub = cursor.parse_next_basic(JitBasicToken::Pub);

        if !cursor.parse_next_basic(JitBasicToken::Fn) {
            return ParseResult::no_match(Self::KIND);
        }

        let name = ident_or_error!(cursor, "expected function name");

        let mut ty_markers = Vec::new();

        if cursor.parse_next_basic(JitBasicToken::LeftAngBracket) {
            while !cursor.peek_next_basic(JitBasicToken::RightAngBracket) {
                let arg = get_required_val!(cursor, TreeTypeMarker::parse(cursor.clone()));
                ty_markers.push(arg);

                let has_comma = cursor.parse_next_basic(JitBasicToken::Comma);

                if !has_comma && !cursor.peek_next_basic(JitBasicToken::RightAngBracket) {
                    return ParseResult::error("Expected a comma");
                }
            }
            cursor.parse_next_basic(JitBasicToken::RightAngBracket);
        }

        let mut args_cursor = pass_val!(cursor.parse_next_group(JitGroupKind::Parentheses));
        let mut args = Vec::new();

        while !args_cursor.is_empty() {
            let arg = get_required_val!(args_cursor, TreeFunctionArg::parse(args_cursor));
            args.push(arg);

            let next_comma = args_cursor.parse_next_basic(JitBasicToken::Comma);
            if !next_comma && !args_cursor.is_empty() {
                return ParseResult::error("expected comma");
            }
        }

        let has_type = cursor.parse_next_basic(JitBasicToken::Arrow);
        let ret_type = if has_type {
            let ty = get_required_val!(cursor, TreeType::parse(cursor));
            Some(ty)
        } else {
            None
        };

        let body_cursor = pass_val!(cursor.parse_next_group(JitGroupKind::Braces));
        let body = get_required_val!(TreeBody::parse(body_cursor));

        ParseResult::Ok(
            cursor,
            Self {
                public: is_pub,
                name: TreeFunctionNameWithMarkers {
                    name: name.clone(),
                    ty_markers,
                },
                args,
                ret_type,
                body,
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct TreeFunctionArg {
    pub name: Cow<'static, str>,
    pub ty: TreeType,
}

impl TreeFunctionArg {
    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let name = ident_or_error!(cursor, "argument name");

        if !cursor.parse_next_basic(JitBasicToken::Colon) {
            return ParseResult::no_match("argument type");
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

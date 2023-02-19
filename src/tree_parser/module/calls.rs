use crate::{
    macro_builder::{JitBasicToken, JitGroupKind, JitToken, JitTokenKind},
    tree_parser::{
        macros::get_required_val,
        parser::{ParseCursor, ParseResult},
    },
};

use super::{ExprLocation, TreeExpression, TreeFunctionNameWithMarkers, TreeTypeMarker};

#[derive(Debug, Clone)]
pub struct TreeStaticFnCall {
    pub name: TreeFunctionNameWithMarkers,
    pub args: Vec<TreeExpression>,
}

impl TreeStaticFnCall {
    const KIND: &'static str = "static function call";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if let Some(JitToken {
            kind: JitTokenKind::Ident(name),
            span: _,
        }) = cursor.next()
        {
            let mut types = Vec::new();

            if cursor.parse_next_basic(JitBasicToken::DoubleColon) {
                if !cursor.parse_next_basic(JitBasicToken::LeftAngBracket) {
                    return ParseResult::error("Expected type arguments");
                }

                while !cursor.peek_next_basic(JitBasicToken::RightAngBracket) {
                    let arg = get_required_val!(cursor, TreeTypeMarker::parse(cursor.clone()));
                    types.push(arg);

                    let has_comma = cursor.parse_next_basic(JitBasicToken::Comma);

                    if !has_comma && !cursor.peek_next_basic(JitBasicToken::RightAngBracket) {
                        return ParseResult::error("Expected a comma");
                    }
                }
                cursor.parse_next_basic(JitBasicToken::RightAngBracket);
            }

            let Ok(mut paren_cursor) = cursor.parse_next_group(JitGroupKind::Parentheses) else {
                return ParseResult::no_match(Self::KIND);
            };

            let mut args = Vec::new();

            while !paren_cursor.is_empty() {
                let arg = get_required_val!(
                    paren_cursor,
                    TreeExpression::parse(paren_cursor.clone(), ExprLocation::BASIC)
                );

                args.push(arg);

                let has_comma = paren_cursor.parse_next_basic(JitBasicToken::Comma);

                if !has_comma && !paren_cursor.is_empty() {
                    return ParseResult::error("Expected a comma");
                }
            }

            ParseResult::Ok(
                cursor,
                Self {
                    name: TreeFunctionNameWithMarkers {
                        name: name.clone(),
                        ty_markers: types,
                    },
                    args,
                },
            )
        } else {
            ParseResult::no_match(Self::KIND)
        }
    }
}

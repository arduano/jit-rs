use std::borrow::Cow;

use crate::{
    macro_builder::{JitBasicToken, JitGroupKind, JitToken, JitTokenKind},
    tree_parser::{
        macros::{get_required_val, pass_val},
        parser::{ParseCursor, ParseResult},
    },
};

use super::{ExprLocation, TreeBinaryOpKind, TreeExpression, TreeType};

#[derive(Debug, Clone)]
pub struct TreePtrAssign {
    pub ptr: Box<TreeExpression>,
    pub value: Box<TreeExpression>,
}

impl TreePtrAssign {
    const KIND: &'static str = "variable assignment";

    pub fn could_match(cursor: &ParseCursor) -> bool {
        cursor.clone().peek_next_basic(JitBasicToken::Equal)
    }

    pub fn parse<'a>(mut cursor: ParseCursor<'a>, ptr: TreeExpression) -> ParseResult<'a, Self> {
        if !cursor.parse_next_basic(JitBasicToken::Equal) {
            return ParseResult::no_match(Self::KIND);
        }

        let value = get_required_val!(cursor, TreeExpression::parse(cursor, ExprLocation::BASIC));

        ParseResult::Ok(
            cursor,
            Self {
                ptr: Box::new(ptr),
                value: Box::new(value),
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct TreeCast {
    pub value: Box<TreeExpression>,
    pub new_ty: TreeType,
}

impl TreeCast {
    const KIND: &'static str = "value cast";

    pub fn could_match(cursor: &ParseCursor) -> bool {
        cursor.clone().peek_next_basic(JitBasicToken::As)
    }

    pub fn parse<'a>(mut cursor: ParseCursor<'a>, ptr: TreeExpression) -> ParseResult<'a, Self> {
        if !cursor.parse_next_basic(JitBasicToken::As) {
            return ParseResult::no_match(Self::KIND);
        }

        let new_ty = get_required_val!(cursor, TreeType::parse(cursor));

        ParseResult::Ok(
            cursor,
            Self {
                value: Box::new(ptr),
                new_ty,
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct TreeBinaryOpList {
    pub pairs: Vec<(TreeExpression, TreeBinaryOpKind)>,
    pub last: Box<TreeExpression>,
}

impl TreeBinaryOpList {
    const KIND: &'static str = "binary operation list";

    pub fn could_match(cursor: &ParseCursor) -> bool {
        TreeBinaryOpKind::parse(cursor.clone()).is_ok()
    }

    pub fn parse<'a>(
        mut cursor: ParseCursor<'a>,
        first: TreeExpression,
        pos: ExprLocation,
    ) -> ParseResult<'a, Self> {
        let Some(first_op) = pass_val!(cursor, TreeBinaryOpKind::parse(cursor.clone())) else {
            return ParseResult::no_match(Self::KIND);
        };

        let inner_pos = pos - ExprLocation::ALLOW_BINARY_EXPR;

        let mut pairs = vec![(first, first_op)];
        let mut last = get_required_val!(cursor, TreeExpression::parse(cursor.clone(), inner_pos));

        loop {
            let Some(op) = pass_val!(cursor, TreeBinaryOpKind::parse(cursor.clone())) else {
                break;
            };

            let right = get_required_val!(cursor, TreeExpression::parse(cursor.clone(), inner_pos));

            pairs.push((last, op));
            last = right;
        }

        ParseResult::Ok(
            cursor,
            Self {
                pairs,
                last: Box::new(last),
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct TreeIndexOp {
    pub ptr: Box<TreeExpression>,
    pub index: Box<TreeExpression>,
}

impl TreeIndexOp {
    const KIND: &'static str = "index operation";

    pub fn could_match(cursor: &ParseCursor) -> bool {
        cursor
            .clone()
            .parse_next_group(JitGroupKind::Brackets)
            .is_ok()
    }

    pub fn parse<'a>(mut cursor: ParseCursor<'a>, value: TreeExpression) -> ParseResult<'a, Self> {
        if let Ok(mut inner_cursor) = cursor.parse_next_group(JitGroupKind::Brackets) {
            let inner = get_required_val!(
                inner_cursor,
                TreeExpression::parse(inner_cursor, ExprLocation::BASIC)
            );

            if !inner_cursor.is_empty() {
                return ParseResult::error("expected end of brackets");
            }

            ParseResult::Ok(
                cursor,
                Self {
                    ptr: Box::new(value),
                    index: Box::new(inner),
                },
            )
        } else {
            ParseResult::no_match(Self::KIND)
        }
    }
}

#[derive(Debug, Clone)]
pub struct TreeIndexField {
    pub ptr: Box<TreeExpression>,
    pub field_name: Cow<'static, str>,
}

impl TreeIndexField {
    const KIND: &'static str = "get field";

    pub fn could_match(cursor: &ParseCursor) -> bool {
        let mut cursor = cursor.clone();
        cursor.parse_next_basic(JitBasicToken::Dot)
            && matches!(
                cursor.next(),
                Some(JitToken {
                    kind: JitTokenKind::Ident(_),
                    ..
                })
            )
    }

    pub fn parse<'a>(mut cursor: ParseCursor<'a>, value: TreeExpression) -> ParseResult<'a, Self> {
        if !cursor.parse_next_basic(JitBasicToken::Dot) {
            return ParseResult::no_match(Self::KIND);
        }

        let field_name = match cursor.next() {
            Some(JitToken {
                kind: JitTokenKind::Ident(name),
                ..
            }) => name,
            _ => return ParseResult::error("expected field name"),
        };

        ParseResult::Ok(
            cursor,
            Self {
                ptr: Box::new(value),
                field_name: field_name.clone(),
            },
        )
    }
}

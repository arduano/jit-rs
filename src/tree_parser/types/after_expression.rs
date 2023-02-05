use crate::{
    macro_builder::{JitBasicToken, JitGroupKind},
    tree_parser::{
        macros::{get_required_val, pass_val},
        parser::{ParseCursor, ParseResult},
    },
};

use super::{ExprLocation, TreeBinaryOpKind, TreeExpression};

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

        let value = get_required_val!(cursor, TreeExpression::parse(cursor, ExprLocation::Other));

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
pub struct TreeBinaryOpList {
    pub pairs: Vec<(TreeExpression, TreeBinaryOpKind)>,
    pub last: Box<TreeExpression>,
}

impl TreeBinaryOpList {
    const KIND: &'static str = "binary operation list";

    pub fn could_match(cursor: &ParseCursor) -> bool {
        TreeBinaryOpKind::parse(cursor.clone()).is_ok()
    }

    pub fn parse<'a>(mut cursor: ParseCursor<'a>, first: TreeExpression) -> ParseResult<'a, Self> {
        let Some(first_op) = pass_val!(cursor, TreeBinaryOpKind::parse(cursor.clone())) else {
            return ParseResult::no_match(Self::KIND);
        };

        let mut pairs = vec![(first, first_op)];
        let mut last = get_required_val!(
            cursor,
            TreeExpression::parse(cursor.clone(), ExprLocation::Other)
        );

        loop {
            let Some(op) = pass_val!(cursor, TreeBinaryOpKind::parse(cursor.clone())) else {
                break;
            };

            let right = get_required_val!(
                cursor,
                TreeExpression::parse(cursor.clone(), ExprLocation::Other)
            );

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
    const KIND: &'static str = "binary operation list";

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
                TreeExpression::parse(inner_cursor, ExprLocation::Other)
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

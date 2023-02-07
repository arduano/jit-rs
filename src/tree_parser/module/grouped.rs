use crate::{
    macro_builder::{JitBasicToken, JitGroupKind},
    tree_parser::{
        macros::{get_required_val, pass_val},
        parser::{ParseCursor, ParseResult},
    },
};

use super::{ExprLocation, TreeBody, TreeExpression};

#[derive(Debug, Clone)]
pub struct TreeIfStatement {
    pub cond: Box<TreeExpression>,
    pub then: TreeBody,
    pub else_: Option<TreeBody>,
}

impl TreeIfStatement {
    const KIND: &'static str = "if statement";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if !cursor.parse_next_basic(JitBasicToken::If) {
            return ParseResult::no_match(Self::KIND);
        }

        let cond = get_required_val!(cursor, TreeExpression::parse(cursor, ExprLocation::Other));

        let then_cursor = pass_val!(cursor.parse_next_group(JitGroupKind::Braces));
        let then = get_required_val!(cursor, TreeBody::parse(then_cursor));

        let else_ = if cursor.parse_next_basic(JitBasicToken::Else) {
            let else_cursor = pass_val!(cursor.parse_next_group(JitGroupKind::Braces));
            let else_ = get_required_val!(cursor, TreeBody::parse(else_cursor));
            Some(else_)
        } else {
            None
        };

        ParseResult::Ok(
            cursor,
            Self {
                cond: Box::new(cond),
                then,
                else_,
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct TreeWhileStatement {
    pub cond: Box<TreeExpression>,
    pub body: TreeBody,
}

impl TreeWhileStatement {
    const KIND: &'static str = "while statement";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        if !cursor.parse_next_basic(JitBasicToken::While) {
            return ParseResult::no_match(Self::KIND);
        }

        let cond = get_required_val!(cursor, TreeExpression::parse(cursor, ExprLocation::Other));

        let body_cursor = pass_val!(cursor.parse_next_group(JitGroupKind::Braces));
        let body = get_required_val!(cursor, TreeBody::parse(body_cursor));

        ParseResult::Ok(
            cursor,
            Self {
                cond: Box::new(cond),
                body,
            },
        )
    }
}

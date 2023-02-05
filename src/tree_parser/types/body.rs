use crate::tree_parser::{
    macros::get_required_val,
    parser::{ParseCursor, ParseResult},
};

use super::{TreeExpression, ExprLocation};

#[derive(Debug, Clone)]
pub struct TreeBody {
    pub body: Vec<TreeExpression>,
}

impl TreeBody {
    const KIND: &'static str = "body";

    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let mut body = Vec::new();

        while !cursor.is_empty() {
            let expr = get_required_val!(cursor, TreeExpression::parse(cursor, ExprLocation::Root));
            body.push(expr);
        }

        ParseResult::Ok(cursor, TreeBody { body })
    }
}

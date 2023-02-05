use std::borrow::Cow;

mod literals;
pub use literals::*;
mod ty;
pub use ty::*;
mod operators;
pub use operators::*;
mod grouped;
pub use grouped::*;
mod expression;
pub use expression::*;
mod after_expression;
pub use after_expression::*;
mod body;
pub use body::*;
mod function;
pub use function::*;

use super::{
    macros::get_required_val,
    parser::{ParseCursor, ParseResult},
};

#[derive(Debug, Clone)]
pub struct TreeModule {
    pub functions: Vec<TreeFunction>,
}

impl TreeModule {
    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let mut functions = Vec::new();

        while !cursor.is_empty() {
            let func = get_required_val!(cursor, TreeFunction::parse(cursor));
            functions.push(func);
        }

        ParseResult::Ok(cursor, TreeModule { functions })
    }
}

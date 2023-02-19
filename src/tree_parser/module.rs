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
mod calls;
pub use calls::*;
mod structs;
pub use structs::*;
mod constants;
pub use constants::*;

use super::{
    macros::pass_val,
    parser::{ParseCursor, ParseResult},
};

#[derive(Debug, Clone)]
pub struct TreeModule {
    pub consts: Vec<TreeConstant>,
    pub structs: Vec<TreeStruct>,
    pub functions: Vec<TreeFunction>,
}

impl TreeModule {
    pub fn parse<'a>(mut cursor: ParseCursor<'a>) -> ParseResult<'a, Self> {
        let mut functions = Vec::new();
        let mut structs = Vec::new();
        let mut consts = Vec::new();

        while !cursor.is_empty() {
            if let Some(func) = pass_val!(cursor, TreeFunction::parse(cursor.clone())) {
                functions.push(func);
            } else if let Some(struc) = pass_val!(cursor, TreeStruct::parse(cursor.clone())) {
                structs.push(struc);
            } else if let Some(cons) = pass_val!(cursor, TreeConstant::parse(cursor.clone())) {
                consts.push(cons);
            } else {
                dbg!(cursor.peek(0));
                return ParseResult::error("expected const, function or struct");
            }
        }

        ParseResult::Ok(
            cursor,
            TreeModule {
                functions,
                structs,
                consts,
            },
        )
    }
}

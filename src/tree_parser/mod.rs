mod macros;
mod parser;
mod module;

pub use module::*;

use crate::macro_builder::JitTokenTree;

use self::parser::{ParseCursor, ParseError};

pub fn parse_tokens_to_tree(tokens: &JitTokenTree) -> Result<TreeModule, ParseError> {
    let cursor = ParseCursor::new(tokens);
    TreeModule::parse(cursor).into_result()
}

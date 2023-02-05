use std::borrow::Cow;

use crate::{
    macro_builder::{JitBasicToken, JitToken, JitTokenKind},
    tree_parser::parser::{ParseCursor, ParseResult, TreeParseItem},
};

#[derive(Debug, Clone)]
pub enum TreeType {
    Base(BaseTreeType),
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

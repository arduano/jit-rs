use std::borrow::Cow;

use crate::tree_parser::TreeConstant;

use super::{mir_parse_literal, MirLiteral};

pub struct MirConstant {
    pub name: Cow<'static, str>,
    pub value: MirLiteral,
}

pub fn mir_parse_constant(cons: &TreeConstant) -> Result<MirConstant, ()> {
    Ok(MirConstant {
        name: cons.name.clone(),
        value: mir_parse_literal(&cons.value)?,
    })
}

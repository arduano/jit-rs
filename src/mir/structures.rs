use std::{borrow::Cow, collections::HashMap};

use super::{MirFunction, MirStatement, MirType};

#[derive(Debug, Clone)]
pub struct MirModule {
    pub functions: Vec<MirFunction>,
    pub structs: Vec<MirStruct>,
}

#[derive(Debug, Clone)]
pub struct MirStructDeclaration {
    pub name: Cow<'static, str>,
    pub field_indexes: HashMap<Cow<'static, str>, usize>,
}

#[derive(Debug, Clone)]
pub struct MirStruct {
    pub decl: MirStructDeclaration,
    pub name: Cow<'static, str>,
    pub fields: Vec<MirType>,
}

#[derive(Debug, Clone)]
pub struct MirVariable {
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct MirBlock {
    pub statements: Vec<MirStatement>,
}

#[derive(Debug, Clone)]
pub struct MirVariableDeclare {
    pub ty: MirType,
}

#[derive(Debug, Clone)]
pub struct MirVariableId {
    pub index: usize,
}

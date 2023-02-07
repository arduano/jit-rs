use std::borrow::Cow;

use super::{MirStatement, MirType};

#[derive(Debug, Clone)]
pub struct MirModule {
    pub functions: Vec<MirFunction>,
}

#[derive(Debug, Clone)]
pub struct MirFunctionDeclaration {
    pub name: Cow<'static, str>,
    pub args: Vec<MirFunctionArg>,
    pub ret_type: MirType,
}

#[derive(Debug, Clone)]
pub struct MirFunctionBody {
    pub decl: MirFunctionDeclaration,
    pub variables: Vec<MirVariableDeclare>,
    pub blocks: Vec<MirBlock>,
}

#[derive(Debug, Clone)]
pub struct MirFunction {
    pub decl: MirFunctionDeclaration,
    pub variables: Vec<MirVariableDeclare>,
    pub blocks: Vec<MirBlock>,
}

#[derive(Debug, Clone)]
pub struct MirFunctionArg {
    pub name: Cow<'static, str>,
    pub ty: MirType,
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

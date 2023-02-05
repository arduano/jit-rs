use std::borrow::Cow;

use super::{MirIntrinsicBinaryOp, MirIntrinsicType, MirLiteral};

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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MirType {
    pub kind: MirTypeKind,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum MirTypeKind {
    Intrinsic(MirIntrinsicType),
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
pub struct MirStatement {
    pub kind: MirStatementKind,
}

#[derive(Debug, Clone)]
pub enum MirStatementKind {
    VarAssign(MirVariableAssign),
    Jump(MirJump),
    ConditionalJump(MirConditionalJump),
    Return(MirExpression),
}

#[derive(Debug, Clone)]
pub struct MirExpression {
    pub kind: MirExpressionKind,
    pub ty: MirType,
}

#[derive(Debug, Clone)]
pub enum MirExpressionKind {
    ReadArg(MirReadArg),
    IndexPtr(Box<MirIndexPtr>),
    ReadVariable(MirReadVariable),
    Literal(MirLiteral),
    BinaryOp(Box<MirBinaryOp>),
    NoValue,
}

#[derive(Debug, Clone)]
pub struct MirVariableDeclare {
    pub ty: MirType,
}

#[derive(Debug, Clone)]
pub struct MirVariableAssign {
    pub var: MirVariable,
    pub value: MirExpression,
}

#[derive(Debug, Clone)]
pub struct MirVariableId {
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct MirReadVariable {
    pub var: MirVariable,
}

#[derive(Debug, Clone)]
pub struct MirReadArg {
    pub index: u32,
}

#[derive(Debug, Clone)]
pub struct MirIndexPtr {
    pub value: MirExpression,
    pub index: MirExpression,
}

#[derive(Debug, Clone)]
pub struct MirJump {
    pub index: usize,
}
#[derive(Debug, Clone)]
pub struct MirConditionalJump {
    pub condition: MirExpression,
    pub then_index: usize,
    pub else_index: usize,
}

#[derive(Debug, Clone)]
pub struct MirBinaryOp {
    pub lhs: MirExpression,
    pub rhs: MirExpression,
    pub op: MirIntrinsicBinaryOp,
}

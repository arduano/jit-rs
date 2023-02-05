use std::borrow::Cow;

use crate::macro_builder::JitTokenNumberKind;

#[derive(Debug, Clone)]
pub struct TreeModule {
    pub functions: Vec<TreeFunction>,
}

#[derive(Debug, Clone)]
pub struct TreeFunction {
    pub name: Cow<'static, str>,
    pub args: Vec<TreeVariableDeclare>,
    pub ret_type: Option<TreeType>,
    pub body: TreeBody,
}

#[derive(Debug, Clone)]
pub struct TreeVariableDeclare {
    pub name: Cow<'static, str>,
    pub ty: TreeType,
}

#[derive(Debug, Clone)]
pub struct TreeType {
    pub is_ptr: bool,
    pub name: Cow<'static, str>,
}

#[derive(Debug, Clone)]
pub struct TreeBody {
    pub body: Vec<TreeExpression>,
}

#[derive(Debug, Clone)]
pub struct TreeExpression {
    pub has_semi: bool,
    pub kind: TreeExpressionKind,
}

#[derive(Debug, Clone)]
pub enum TreeExpressionKind {
    IfStatement(TreeIfStatement),
    WhileStatement(TreeWhileStatement),
    LetStatement(TreeLetStatement),
    ReturnStatement(TreeReturnStatement),
    BinaryOpList(TreeBinaryOpList),
    IndexOp(TreeIndexOp),
    UnaryOp(TreeUnaryOp),
    Group(TreeBody),
    ReadVar(TreeVarRead),
    VarAssign(TreeVarAssign),
    Number(TreeNumberLiteral),
    Bool(TreeBoolLiteral),
    Parenthesized(Box<TreeExpression>),
}

#[derive(Debug, Clone)]
pub struct TreeIfStatement {
    pub cond: Box<TreeExpression>,
    pub then: TreeBody,
    pub else_: Option<TreeBody>,
}

#[derive(Debug, Clone)]
pub struct TreeWhileStatement {
    pub cond: Box<TreeExpression>,
    pub body: TreeBody,
}

#[derive(Debug, Clone)]
pub struct TreeLetStatement {
    pub name: Cow<'static, str>,
    pub value: Box<TreeExpression>,
}

#[derive(Debug, Clone)]
pub struct TreeReturnStatement {
    pub value: Box<TreeExpression>,
}

#[derive(Debug, Clone)]
pub struct TreeVarAssign {
    pub name: Cow<'static, str>,
    pub value: Box<TreeExpression>,
}

#[derive(Debug, Clone)]
pub struct TreeVarRead {
    pub name: Cow<'static, str>,
}

#[derive(Debug, Clone)]
pub struct TreeBinaryOpList {
    pub pairs: Vec<(TreeExpression, TreeBinaryOpKind)>,
    pub last: Box<TreeExpression>,
}

#[derive(Debug, Clone)]
pub struct TreeUnaryOp {
    pub op: TreeUnaryOpKind,
    pub expr: Box<TreeExpression>,
}

#[derive(Debug, Clone)]
pub struct TreeIndexOp {
    pub value: Box<TreeExpression>,
    pub index: Box<TreeExpression>,
}

#[derive(Debug, Clone, Copy)]
pub enum TreeBinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Gt,
    Eq,
    Neq,
    Lte,
    Gte,
    BinaryAnd,
    BinaryOr,
    BinaryXor,
}

#[derive(Debug, Clone)]
pub enum TreeUnaryOpKind {
    Not,
    Neg,
}

#[derive(Debug, Clone)]
pub struct TreeNumberLiteral {
    pub value: Cow<'static, str>,
    pub ty: JitTokenNumberKind,
}

#[derive(Debug, Clone)]
pub struct TreeBoolLiteral {
    pub value: bool,
}

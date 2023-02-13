use std::borrow::Cow;

use crate::common::NumberKind;

use super::{
    variables::MirVariableDecl, MirIntrinsicBinaryOp, MirIntrinsicOp, MirIntrinsicUnaryOp,
    MirIntrinsicVectorBinaryOp, MirLiteral, MirType, MirVariable,
};

#[derive(Debug, Clone)]
pub struct MirExpression {
    pub kind: MirExpressionKind,
    pub ty: MirType,
}

impl MirExpression {
    pub fn deref_variable(var: MirVariableDecl) -> Self {
        let ptr = MirExpression {
            kind: MirExpressionKind::GetVariablePtr(MirGetVariablePtr { var: var.as_var() }),
            ty: var.ty.clone(),
        };

        Self {
            kind: MirExpressionKind::PtrDeref(Box::new(MirPtrDeref {
                ptr,
                underlying_ty: var.ty.clone(),
            })),
            ty: var.ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum MirExpressionKind {
    ReadArg(MirReadArg),
    IndexPtr(Box<MirIndexPtr>),
    IndexStruct(Box<MirIndexStruct>),
    GetVariablePtr(MirGetVariablePtr),
    PtrDeref(Box<MirPtrDeref>),
    CastNumber(MirCastNumber),
    CastVector(MirCastVector),
    Literal(MirLiteral),
    BinaryOp(Box<MirBinaryOp>),
    VectorBinaryOp(Box<MirVectorBinaryOp>),
    UnaryOp(Box<MirUnaryOp>),
    PtrCast(Box<MirExpression>),
    IntrinsicOp(Box<MirIntrinsicOp>),
    StructInit(MirStructInit),
    FunctionCall(MirFunctionCall),
    NoValue,
}

#[derive(Debug, Clone)]
pub struct MirBinaryOp {
    pub lhs: MirExpression,
    pub rhs: MirExpression,
    pub op: MirIntrinsicBinaryOp,
}

#[derive(Debug, Clone)]
pub struct MirVectorBinaryOp {
    pub lhs: MirExpression,
    pub rhs: MirExpression,
    pub op: MirIntrinsicVectorBinaryOp,
    pub scalar_ty: NumberKind,
    pub width: u32,
}

#[derive(Debug, Clone)]
pub struct MirUnaryOp {
    pub operand: MirExpression,
    pub op: MirIntrinsicUnaryOp,
}

#[derive(Debug, Clone)]
pub struct MirPtrDeref {
    pub underlying_ty: MirType,
    pub ptr: MirExpression,
}

#[derive(Debug, Clone)]
pub struct MirCastNumber {
    pub from: NumberKind,
    pub to: NumberKind,
    pub number: Box<MirExpression>,
}

#[derive(Debug, Clone)]
pub struct MirCastVector {
    pub from: NumberKind,
    pub to: NumberKind,
    pub vector: Box<MirExpression>,
}

#[derive(Debug, Clone)]
pub struct MirGetVariablePtr {
    pub var: MirVariable,
}

#[derive(Debug, Clone)]
pub struct MirIndexPtr {
    pub value: MirExpression,
    pub index: MirExpression,
    pub index_ty: MirType,
}

#[derive(Debug, Clone)]
pub struct MirIndexStruct {
    pub value: MirExpression,
    pub index: u32,
    pub struct_name: Cow<'static, str>,
    pub index_ty: MirType,
}

#[derive(Debug, Clone)]
pub struct MirReadArg {
    pub index: u32,
}

#[derive(Debug, Clone)]
pub struct MirFunctionCall {
    pub is_void: bool,
    pub name: Cow<'static, str>,
    pub args: Vec<MirExpression>,
}

#[derive(Debug, Clone)]
pub struct MirStructInit {
    pub name: Cow<'static, str>,
    pub fields: Vec<MirExpression>,
}

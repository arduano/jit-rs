use crate::common::NumberKind;

use super::{
    variables::MirVariableDecl, MirIntrinsicBinaryOp, MirIntrinsicUnaryOp,
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
    GetVariablePtr(MirGetVariablePtr),
    PtrDeref(Box<MirPtrDeref>),
    Literal(MirLiteral),
    BinaryOp(Box<MirBinaryOp>),
    VectorBinaryOp(Box<MirVectorBinaryOp>),
    UnaryOp(Box<MirUnaryOp>),
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
pub struct MirReadArg {
    pub index: u32,
}

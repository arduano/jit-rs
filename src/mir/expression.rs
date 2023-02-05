use super::{
    variables::MirVariableDecl, MirIntrinsicBinaryOp, MirLiteral, MirType, MirVariable,
    MirVariableDeclare,
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
            kind: MirExpressionKind::PtrDeref(Box::new(MirPtrDeref { ptr })),
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
    NoValue,
}

#[derive(Debug, Clone)]
pub struct MirBinaryOp {
    pub lhs: MirExpression,
    pub rhs: MirExpression,
    pub op: MirIntrinsicBinaryOp,
}

#[derive(Debug, Clone)]
pub struct MirPtrDeref {
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
}

#[derive(Debug, Clone)]
pub struct MirReadArg {
    pub index: u32,
}

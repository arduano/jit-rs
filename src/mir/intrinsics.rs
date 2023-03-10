use crate::common::NumberKind;

use super::{MirExpression, MirType};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirIntrinsicBinaryOp {
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    UIntDiv,
    IntRem,
    UIntRem,
    IntShl,
    IntShr,
    UIntShr,
    IntAnd,
    IntOr,
    IntXor,
    IntEq,
    IntNeq,
    IntLt,
    IntLte,
    IntGt,
    IntGte,
    UIntLt,
    UIntLte,
    UIntGt,
    UIntGte,
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatRem,
    FloatEq,
    FloatNeq,
    FloatLt,
    FloatLte,
    FloatGt,
    FloatGte,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirIntrinsicVectorBinaryOp {
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    UIntDiv,
    IntRem,
    UIntRem,
    IntShl,
    IntShr,
    UIntShr,
    IntAnd,
    IntOr,
    IntXor,
    IntMax,
    IntMin,
    IntUMax,
    IntUMin,

    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatRem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirIntrinsicUnaryOp {
    IntNeg,
    FloatNeg,
    BoolNot,
}

#[derive(Debug, Clone)]
pub enum MirIntrinsicOp {
    LoadVector {
        ptr: MirExpression,
        scalar_ty: NumberKind,
        width: u32,
    },
    StoreVector {
        ptr: MirExpression,
        value: MirExpression,
        scalar_ty: NumberKind,
        width: u32,
    },
    ExtendToVector {
        unit_ty: NumberKind,
        unit: MirExpression,
        width: u32,
    },
    Zeroed {
        ty: MirType,
    },
    Box {
        value: MirExpression,
        ty: MirType,
    },
    Drop {
        ptr: MirExpression,
        ty: MirType,
    },
    Unreachable,
}

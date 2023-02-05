use super::MirType;

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
pub enum MirIntrinsicUnaryOp {
    IntNeg,
    FloatNeg,
    BoolNot,
}

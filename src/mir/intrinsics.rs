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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirIntrinsicType {
    Never,
    Void,
    Bool,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MirLiteral {
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum IntBits {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
    BitsSize,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum FloatBits {
    Bits32,
    Bits64,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum NumberKind {
    SignedInt(IntBits),
    UnsignedInt(IntBits),
    Float(FloatBits),
}

impl std::fmt::Display for NumberKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            NumberKind::SignedInt(bits) => match bits {
                IntBits::Bits8 => write!(f, "i8"),
                IntBits::Bits16 => write!(f, "i16"),
                IntBits::Bits32 => write!(f, "i32"),
                IntBits::Bits64 => write!(f, "i64"),
                IntBits::BitsSize => write!(f, "isize"),
            },
            NumberKind::UnsignedInt(bits) => match bits {
                IntBits::Bits8 => write!(f, "u8"),
                IntBits::Bits16 => write!(f, "u16"),
                IntBits::Bits32 => write!(f, "u32"),
                IntBits::Bits64 => write!(f, "u64"),
                IntBits::BitsSize => write!(f, "usize"),
            },
            NumberKind::Float(bits) => match bits {
                FloatBits::Bits32 => write!(f, "f32"),
                FloatBits::Bits64 => write!(f, "f64"),
            },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumberValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),
    F32(f32),
    F64(f64),
}

impl NumberValue {
    pub fn kind(&self) -> NumberKind {
        match self {
            NumberValue::U8(_) => NumberKind::UnsignedInt(IntBits::Bits8),
            NumberValue::U16(_) => NumberKind::UnsignedInt(IntBits::Bits16),
            NumberValue::U32(_) => NumberKind::UnsignedInt(IntBits::Bits32),
            NumberValue::U64(_) => NumberKind::UnsignedInt(IntBits::Bits64),
            NumberValue::USize(_) => NumberKind::UnsignedInt(IntBits::BitsSize),
            NumberValue::I8(_) => NumberKind::SignedInt(IntBits::Bits8),
            NumberValue::I16(_) => NumberKind::SignedInt(IntBits::Bits16),
            NumberValue::I32(_) => NumberKind::SignedInt(IntBits::Bits32),
            NumberValue::I64(_) => NumberKind::SignedInt(IntBits::Bits64),
            NumberValue::ISize(_) => NumberKind::SignedInt(IntBits::BitsSize),
            NumberValue::F32(_) => NumberKind::Float(FloatBits::Bits32),
            NumberValue::F64(_) => NumberKind::Float(FloatBits::Bits64),
        }
    }
}

impl std::fmt::Display for NumberValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            NumberValue::U8(n) => write!(f, "{}", n),
            NumberValue::U16(n) => write!(f, "{}", n),
            NumberValue::U32(n) => write!(f, "{}", n),
            NumberValue::U64(n) => write!(f, "{}", n),
            NumberValue::USize(n) => write!(f, "{}", n),
            NumberValue::I8(n) => write!(f, "{}", n),
            NumberValue::I16(n) => write!(f, "{}", n),
            NumberValue::I32(n) => write!(f, "{}", n),
            NumberValue::I64(n) => write!(f, "{}", n),
            NumberValue::ISize(n) => write!(f, "{}", n),
            NumberValue::F32(n) => write!(f, "{}", n),
            NumberValue::F64(n) => write!(f, "{}", n),
        }
    }
}

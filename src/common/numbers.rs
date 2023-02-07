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

use std::{borrow::Cow, fmt::Formatter};

use crate::{
    common::{FloatBits, IntBits, NumberKind, NumberValue},
    tree_parser::{TreeBoolLiteral, TreeLiteral, TreeNumberLiteral},
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum MirType {
    Never,
    Void,
    Bool,
    Num(NumberKind),
    Ptr(Box<MirType>),
    ConstArray(Box<MirType>, u32),
    Vector(NumberKind, u32),
    Struct(Cow<'static, str>),
}

impl MirType {
    pub fn as_ptr(&self) -> MirType {
        MirType::Ptr(Box::new(self.clone()))
    }

    pub fn deref_ptr(&self) -> &MirType {
        self.try_deref_ptr().expect("Cannot deref non-pointer type")
    }

    pub fn try_deref_ptr(&self) -> Option<&MirType> {
        match self {
            MirType::Ptr(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn is_never(&self) -> bool {
        self == &MirType::Never
    }

    pub fn format_as_mangled_unique(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MirType::Never => write!(f, "n"),
            MirType::Void => write!(f, "v"),
            MirType::Bool => write!(f, "b"),
            MirType::Num(kind) => write!(f, "n{}", kind),
            MirType::Ptr(ty) => {
                write!(f, "p")?;
                ty.format_as_mangled_unique(f)
            }
            MirType::ConstArray(ty, len) => {
                write!(f, "a{}", len)?;
                ty.format_as_mangled_unique(f)
            }
            MirType::Vector(kind, len) => write!(f, "v{}{}", kind, len),
            MirType::Struct(name) => write!(f, "s{}", name),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MirLiteral {
    Bool(bool),
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

impl PartialEq for MirLiteral {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::U8(l0), Self::U8(r0)) => l0 == r0,
            (Self::U16(l0), Self::U16(r0)) => l0 == r0,
            (Self::U32(l0), Self::U32(r0)) => l0 == r0,
            (Self::U64(l0), Self::U64(r0)) => l0 == r0,
            (Self::USize(l0), Self::USize(r0)) => l0 == r0,
            (Self::I8(l0), Self::I8(r0)) => l0 == r0,
            (Self::I16(l0), Self::I16(r0)) => l0 == r0,
            (Self::I32(l0), Self::I32(r0)) => l0 == r0,
            (Self::I64(l0), Self::I64(r0)) => l0 == r0,
            (Self::ISize(l0), Self::ISize(r0)) => l0 == r0,
            (Self::F32(l0), Self::F32(r0)) => l0 == r0 || (l0.is_nan() && r0.is_nan()),
            (Self::F64(l0), Self::F64(r0)) => l0 == r0 || (l0.is_nan() && r0.is_nan()),
            _ => false,
        }
    }
}
impl Eq for MirLiteral {}

impl MirLiteral {
    pub fn format_as_mangled_unique(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MirLiteral::Bool(b) => write!(f, "{}b", b),
            MirLiteral::U8(n) => write!(f, "{}u8", n),
            MirLiteral::U16(n) => write!(f, "{}u16", n),
            MirLiteral::U32(n) => write!(f, "{}u32", n),
            MirLiteral::U64(n) => write!(f, "{}u64", n),
            MirLiteral::USize(n) => write!(f, "{}usize", n),
            MirLiteral::I8(n) => write!(f, "{}i8", n),
            MirLiteral::I16(n) => write!(f, "{}i16", n),
            MirLiteral::I32(n) => write!(f, "{}i32", n),
            MirLiteral::I64(n) => write!(f, "{}i64", n),
            MirLiteral::ISize(n) => write!(f, "{}isize", n),
            MirLiteral::F32(n) => write!(f, "{}f32", n),
            MirLiteral::F64(n) => write!(f, "{}f64", n),
        }
    }

    pub fn ty(&self) -> MirType {
        match self {
            MirLiteral::Bool(_) => MirType::Bool,
            MirLiteral::U8(_) => MirType::Num(NumberKind::UnsignedInt(IntBits::Bits8)),
            MirLiteral::U16(_) => MirType::Num(NumberKind::UnsignedInt(IntBits::Bits16)),
            MirLiteral::U32(_) => MirType::Num(NumberKind::UnsignedInt(IntBits::Bits32)),
            MirLiteral::U64(_) => MirType::Num(NumberKind::UnsignedInt(IntBits::Bits64)),
            MirLiteral::USize(_) => MirType::Num(NumberKind::UnsignedInt(IntBits::BitsSize)),
            MirLiteral::I8(_) => MirType::Num(NumberKind::SignedInt(IntBits::Bits8)),
            MirLiteral::I16(_) => MirType::Num(NumberKind::SignedInt(IntBits::Bits16)),
            MirLiteral::I32(_) => MirType::Num(NumberKind::SignedInt(IntBits::Bits32)),
            MirLiteral::I64(_) => MirType::Num(NumberKind::SignedInt(IntBits::Bits64)),
            MirLiteral::ISize(_) => MirType::Num(NumberKind::SignedInt(IntBits::BitsSize)),
            MirLiteral::F32(_) => MirType::Num(NumberKind::Float(FloatBits::Bits32)),
            MirLiteral::F64(_) => MirType::Num(NumberKind::Float(FloatBits::Bits64)),
        }
    }
}

pub fn mir_parse_literal(lit: &TreeLiteral) -> Result<MirLiteral, ()> {
    match lit {
        TreeLiteral::Number(num) => mir_parse_num_literal(num),
        TreeLiteral::Bool(bool) => mir_parse_bool_literal(bool),
    }
}

pub fn mir_parse_num_literal(num: &TreeNumberLiteral) -> Result<MirLiteral, ()> {
    let literal = match num.value {
        NumberValue::U8(val) => MirLiteral::U8(val),
        NumberValue::U16(val) => MirLiteral::U16(val),
        NumberValue::U32(val) => MirLiteral::U32(val),
        NumberValue::U64(val) => MirLiteral::U64(val),
        NumberValue::USize(val) => MirLiteral::USize(val),
        NumberValue::I8(val) => MirLiteral::I8(val),
        NumberValue::I16(val) => MirLiteral::I16(val),
        NumberValue::I32(val) => MirLiteral::I32(val),
        NumberValue::I64(val) => MirLiteral::I64(val),
        NumberValue::ISize(val) => MirLiteral::ISize(val),
        NumberValue::F32(val) => MirLiteral::F32(val),
        NumberValue::F64(val) => MirLiteral::F64(val),
    };

    Ok(literal)
}

pub fn mir_parse_bool_literal(bool: &TreeBoolLiteral) -> Result<MirLiteral, ()> {
    Ok(MirLiteral::Bool(bool.value))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MirTypeMarker {
    Type(MirType),
    Literal(MirLiteral),
}

impl MirTypeMarker {
    pub fn format_as_mangled_unique(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MirTypeMarker::Type(ty) => {
                write!(f, "t")?;
                ty.format_as_mangled_unique(f)
            }
            MirTypeMarker::Literal(lit) => {
                write!(f, "l")?;
                lit.format_as_mangled_unique(f)
            }
        }
    }
}

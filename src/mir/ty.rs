use std::{borrow::Cow, fmt::Formatter};

use crate::{
    common::{FloatBits, IntBits, NumberKind},
    tree_parser::{TreeBoolLiteral, TreeNumberLiteral},
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

#[derive(Debug, Clone, Copy, PartialEq)]
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
}

pub fn mir_parse_num_literal(num: &TreeNumberLiteral) -> MirLiteral {
    use FloatBits as FB;
    use IntBits as IB;
    use MirLiteral as Lit;
    use NumberKind::*;

    // FIXME: Parse numbers with underscores

    let value = &num.value;
    let literal = match num.ty {
        UnsignedInt(IB::Bits8) => Lit::U8(value.parse().unwrap()),
        UnsignedInt(IB::Bits16) => Lit::U16(value.parse().unwrap()),
        UnsignedInt(IB::Bits32) => Lit::U32(
            value
                .parse()
                .expect(&format!("Failed to parse {} as u32", value)),
        ),
        UnsignedInt(IB::Bits64) => Lit::U64(value.parse().unwrap()),
        UnsignedInt(IB::BitsSize) => Lit::USize(value.parse().unwrap()),
        SignedInt(IB::Bits8) => Lit::I8(value.parse().unwrap()),
        SignedInt(IB::Bits16) => Lit::I16(value.parse().unwrap()),
        SignedInt(IB::Bits32) => Lit::I32(value.parse().unwrap()),
        SignedInt(IB::Bits64) => Lit::I64(value.parse().unwrap()),
        SignedInt(IB::BitsSize) => Lit::ISize(value.parse().unwrap()),
        Float(FB::Bits32) => Lit::F32(value.parse().unwrap()),
        Float(FB::Bits64) => Lit::F64(value.parse().unwrap()),
    };

    literal
}

pub fn mir_parse_bool_literal(bool: &TreeBoolLiteral) -> MirLiteral {
    MirLiteral::Bool(bool.value)
}

#[derive(Debug, Clone, PartialEq)]
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

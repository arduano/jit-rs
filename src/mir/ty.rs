use std::borrow::Cow;

use crate::common::NumberKind;

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

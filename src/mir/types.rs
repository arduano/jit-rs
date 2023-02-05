#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MirType {
    pub kind: MirTypeKind,
}

impl MirType {
    pub fn as_ptr(&self) -> MirType {
        MirType {
            kind: MirTypeKind::Intrinsic(MirIntrinsicType::Ptr(Box::new(self.clone()))),
        }
    }

    pub fn deref_ptr(&self) -> &MirType {
        match &self.kind {
            MirTypeKind::Intrinsic(MirIntrinsicType::Ptr(ty)) => ty,
            _ => panic!("Cannot deref non-pointer type"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum MirTypeKind {
    Intrinsic(MirIntrinsicType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Ptr(Box<MirType>),
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

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicValueType {
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    // I8Vec(u32),
    // I16Vec(u32),
    // I32Vec(u32),
    // I64Vec(u32),
    // U8Vec(u32),
    // U16Vec(u32),
    // U32Vec(u32),
    // U64Vec(u32),
    // F32Vec(u32),
    // F64Vec(u32),
}

impl IntrinsicValueType {
    pub fn size(&self) -> u32 {
        match self {
            IntrinsicValueType::Bool => 1,
            IntrinsicValueType::I8 => 1,
            IntrinsicValueType::I16 => 2,
            IntrinsicValueType::I32 => 4,
            IntrinsicValueType::I64 => 8,
            IntrinsicValueType::U8 => 1,
            IntrinsicValueType::U16 => 2,
            IntrinsicValueType::U32 => 4,
            IntrinsicValueType::U64 => 8,
            IntrinsicValueType::F32 => 4,
            IntrinsicValueType::F64 => 8,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            IntrinsicValueType::Bool => "bool",
            IntrinsicValueType::I8 => "i8",
            IntrinsicValueType::I16 => "i16",
            IntrinsicValueType::I32 => "i32",
            IntrinsicValueType::I64 => "i64",
            IntrinsicValueType::U8 => "u8",
            IntrinsicValueType::U16 => "u16",
            IntrinsicValueType::U32 => "u32",
            IntrinsicValueType::U64 => "u64",
            IntrinsicValueType::F32 => "f32",
            IntrinsicValueType::F64 => "f64",
        }
    }
}

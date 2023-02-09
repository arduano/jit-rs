use inkwell::values::{BasicValue, BasicValueEnum};

use crate::{
    common::{FloatBits, IntBits, NumberKind},
    mir::{MirCastNumber, MirType},
};

use super::{FunctionInsertContext, SizeBits};

enum CastNumberKind {
    IntTruncate,
    SIntExtend,
    UIntExtend,
    SIntToFloat,
    UIntToFloat,
    FloatToSInt,
    FloatToUInt,
    FloatTruncate,
    FloatExtend,
}

fn get_int_bits(bits: IntBits, size_bits: SizeBits) -> u32 {
    match bits {
        IntBits::Bits8 => 8,
        IntBits::Bits16 => 16,
        IntBits::Bits32 => 32,
        IntBits::Bits64 => 64,
        IntBits::BitsSize => match size_bits {
            SizeBits::Bits32 => 32,
            SizeBits::Bits64 => 64,
        },
    }
}

fn get_float_bits(bits: FloatBits) -> u32 {
    match bits {
        FloatBits::Bits32 => 32,
        FloatBits::Bits64 => 64,
    }
}

fn choose_cast_kind(
    from: NumberKind,
    to: NumberKind,
    size_bits: SizeBits,
) -> Option<CastNumberKind> {
    match (from, to) {
        (NumberKind::SignedInt(from), NumberKind::SignedInt(to))
        | (NumberKind::SignedInt(from), NumberKind::UnsignedInt(to)) => {
            let from_bits = get_int_bits(from, size_bits);
            let to_bits = get_int_bits(to, size_bits);

            if from_bits > to_bits {
                Some(CastNumberKind::IntTruncate)
            } else if from_bits < to_bits {
                Some(CastNumberKind::SIntExtend)
            } else {
                None
            }
        }
        (NumberKind::UnsignedInt(from), NumberKind::UnsignedInt(to))
        | (NumberKind::UnsignedInt(from), NumberKind::SignedInt(to)) => {
            let from_bits = get_int_bits(from, size_bits);
            let to_bits = get_int_bits(to, size_bits);

            if from_bits > to_bits {
                Some(CastNumberKind::IntTruncate)
            } else if from_bits < to_bits {
                Some(CastNumberKind::UIntExtend)
            } else {
                None
            }
        }
        (NumberKind::Float(from), NumberKind::Float(to)) => {
            let from_bits = get_float_bits(from);
            let to_bits = get_float_bits(to);

            if from_bits > to_bits {
                Some(CastNumberKind::FloatTruncate)
            } else if from_bits < to_bits {
                Some(CastNumberKind::FloatExtend)
            } else {
                None
            }
        }
        (NumberKind::SignedInt(_), NumberKind::Float(_)) => Some(CastNumberKind::SIntToFloat),
        (NumberKind::UnsignedInt(_), NumberKind::Float(_)) => Some(CastNumberKind::UIntToFloat),
        (NumberKind::Float(_), NumberKind::SignedInt(_)) => Some(CastNumberKind::FloatToSInt),
        (NumberKind::Float(_), NumberKind::UnsignedInt(_)) => Some(CastNumberKind::FloatToUInt),
    }
}

pub fn codegen_number_cast_expr<'ctx>(
    op: &MirCastNumber,
    ctx: &mut FunctionInsertContext<'ctx, '_>,
) -> BasicValueEnum<'ctx> {
    let value = ctx.write_expression(&op.number).unwrap();

    let kind = choose_cast_kind(op.from, op.to, ctx.module.size_bits);

    let Some(kind) = kind else {
        return value;
    };

    let to_ty = ctx.get_type(&MirType::Num(op.to));

    let builder = &ctx.module.builder;

    match kind {
        CastNumberKind::IntTruncate => builder
            .build_int_truncate(value.into_int_value(), to_ty.into_int_type(), "trunc")
            .as_basic_value_enum(),
        CastNumberKind::SIntExtend => builder
            .build_int_s_extend(value.into_int_value(), to_ty.into_int_type(), "sext")
            .as_basic_value_enum(),
        CastNumberKind::UIntExtend => builder
            .build_int_z_extend(value.into_int_value(), to_ty.into_int_type(), "zext")
            .as_basic_value_enum(),
        CastNumberKind::SIntToFloat => builder
            .build_signed_int_to_float(value.into_int_value(), to_ty.into_float_type(), "sitofp")
            .as_basic_value_enum(),
        CastNumberKind::UIntToFloat => builder
            .build_unsigned_int_to_float(value.into_int_value(), to_ty.into_float_type(), "uitofp")
            .as_basic_value_enum(),
        CastNumberKind::FloatToSInt => builder
            .build_float_to_signed_int(value.into_float_value(), to_ty.into_int_type(), "fptosi")
            .as_basic_value_enum(),
        CastNumberKind::FloatToUInt => builder
            .build_float_to_unsigned_int(value.into_float_value(), to_ty.into_int_type(), "fptoui")
            .as_basic_value_enum(),
        CastNumberKind::FloatTruncate => builder
            .build_float_trunc(value.into_float_value(), to_ty.into_float_type(), "fptrunc")
            .as_basic_value_enum(),
        CastNumberKind::FloatExtend => builder
            .build_float_ext(value.into_float_value(), to_ty.into_float_type(), "fpext")
            .as_basic_value_enum(),
    }
}

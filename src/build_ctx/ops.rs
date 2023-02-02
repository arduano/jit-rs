use inkwell::{
    builder::Builder, context::Context, values::BasicValue, FloatPredicate, IntPredicate,
};

use crate::{
    syntax::{BinaryMathOpKind, ComparisonOpKind, ConstantValue},
    types::IntrinsicValueType,
};

use super::Value;

pub fn build_constant_value<'a>(ctx: &'a Context, val: &ConstantValue) -> Value<'a> {
    use IntrinsicValueType::*;

    match val {
        &ConstantValue::Bool(val) => Value {
            ty: Bool,
            val: ctx
                .bool_type()
                .const_int(val as u64, false)
                .as_basic_value_enum(),
        },
        &ConstantValue::U8(val) => Value {
            ty: U8,
            val: ctx
                .i8_type()
                .const_int(val as u64, false)
                .as_basic_value_enum(),
        },
        &ConstantValue::U16(val) => Value {
            ty: U16,
            val: ctx
                .i16_type()
                .const_int(val as u64, false)
                .as_basic_value_enum(),
        },
        &ConstantValue::U32(val) => Value {
            ty: U32,
            val: ctx
                .i32_type()
                .const_int(val as u64, false)
                .as_basic_value_enum(),
        },
        &ConstantValue::U64(val) => Value {
            ty: U64,
            val: ctx
                .i64_type()
                .const_int(val as u64, false)
                .as_basic_value_enum(),
        },
        &ConstantValue::I8(val) => Value {
            ty: I8,
            val: ctx
                .i8_type()
                .const_int(val as u64, true)
                .as_basic_value_enum(),
        },
        &ConstantValue::I16(val) => Value {
            ty: I16,
            val: ctx
                .i16_type()
                .const_int(val as u64, true)
                .as_basic_value_enum(),
        },
        &ConstantValue::I32(val) => Value {
            ty: I32,
            val: ctx
                .i32_type()
                .const_int(val as u64, true)
                .as_basic_value_enum(),
        },
        &ConstantValue::I64(val) => Value {
            ty: I64,
            val: ctx
                .i64_type()
                .const_int(val as u64, true)
                .as_basic_value_enum(),
        },
        &ConstantValue::F32(val) => Value {
            ty: F32,
            val: ctx.f32_type().const_float(val as f64).as_basic_value_enum(),
        },
        &ConstantValue::F64(val) => Value {
            ty: F64,
            val: ctx.f64_type().const_float(val as f64).as_basic_value_enum(),
        },
    }
}

pub fn build_binary_math_op<'a>(
    builder: &Builder<'a>,
    lhs: Value<'a>,
    rhs: Value<'a>,
    op: BinaryMathOpKind,
) -> Value<'a> {
    use BinaryMathOpKind as BO;
    use IntrinsicValueType::*;

    match lhs.ty {
        I8 | I16 | I32 | I64 => {
            if lhs.ty != rhs.ty {
                panic!()
            }

            let ty = lhs.ty;

            let lhs = lhs.val.into_int_value();
            let rhs = rhs.val.into_int_value();

            let result = match op {
                BO::Add => builder.build_int_add(lhs, rhs, "add"),
                BO::Sub => builder.build_int_sub(lhs, rhs, "sub"),
                BO::Mul => builder.build_int_mul(lhs, rhs, "mul"),
                BO::Div => builder.build_int_signed_div(lhs, rhs, "div"),
            };

            Value {
                ty,
                val: result.as_basic_value_enum(),
            }
        }
        U8 | U16 | U32 | U64 => {
            if lhs.ty != rhs.ty {
                panic!()
            }

            let ty = lhs.ty;

            let lhs = lhs.val.into_int_value();
            let rhs = rhs.val.into_int_value();

            let result = match op {
                BO::Add => builder.build_int_add(lhs, rhs, "add"),
                BO::Sub => builder.build_int_sub(lhs, rhs, "sub"),
                BO::Mul => builder.build_int_mul(lhs, rhs, "mul"),
                BO::Div => builder.build_int_unsigned_div(lhs, rhs, "div"),
            };

            Value {
                ty,
                val: result.as_basic_value_enum(),
            }
        }
        F32 | F64 => {
            if lhs.ty != rhs.ty {
                panic!()
            }

            let ty = lhs.ty;

            let lhs = lhs.val.into_float_value();
            let rhs = rhs.val.into_float_value();

            let result = match op {
                BO::Add => builder.build_float_add(lhs, rhs, "add"),
                BO::Sub => builder.build_float_sub(lhs, rhs, "sub"),
                BO::Mul => builder.build_float_mul(lhs, rhs, "mul"),
                BO::Div => builder.build_float_div(lhs, rhs, "div"),
            };

            Value {
                ty,
                val: result.as_basic_value_enum(),
            }
        }
        _ => panic!(),
    }
}

pub fn build_comparison_op<'a>(
    builder: &Builder<'a>,
    lhs: Value<'a>,
    rhs: Value<'a>,
    op: ComparisonOpKind,
) -> Value<'a> {
    use ComparisonOpKind as CO;
    use IntrinsicValueType::*;

    match lhs.ty {
        I8 | I16 | I32 | I64 => {
            if lhs.ty != rhs.ty {
                panic!()
            }

            let lhs = lhs.val.into_int_value();
            let rhs = rhs.val.into_int_value();

            let result = match op {
                CO::Eq => builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "eq"),
                CO::Ne => builder.build_int_compare(IntPredicate::NE, lhs, rhs, "ne"),
                CO::Lt => builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "lt"),
                CO::Le => builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "le"),
                CO::Gt => builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "gt"),
                CO::Ge => builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "ge"),
            };

            Value {
                ty: IntrinsicValueType::Bool,
                val: result.as_basic_value_enum(),
            }
        }
        U8 | U16 | U32 | U64 => {
            if lhs.ty != rhs.ty {
                panic!()
            }

            let lhs = lhs.val.into_int_value();
            let rhs = rhs.val.into_int_value();

            let result = match op {
                CO::Eq => builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "eq"),
                CO::Ne => builder.build_int_compare(IntPredicate::NE, lhs, rhs, "ne"),
                CO::Lt => builder.build_int_compare(IntPredicate::ULT, lhs, rhs, "lt"),
                CO::Le => builder.build_int_compare(IntPredicate::ULE, lhs, rhs, "le"),
                CO::Gt => builder.build_int_compare(IntPredicate::UGT, lhs, rhs, "gt"),
                CO::Ge => builder.build_int_compare(IntPredicate::UGE, lhs, rhs, "ge"),
            };

            Value {
                ty: IntrinsicValueType::Bool,
                val: result.as_basic_value_enum(),
            }
        }
        F32 | F64 => {
            if lhs.ty != rhs.ty {
                panic!()
            }

            let lhs = lhs.val.into_float_value();
            let rhs = rhs.val.into_float_value();

            let result = match op {
                CO::Eq => builder.build_float_compare(FloatPredicate::OEQ, lhs, rhs, "eq"),
                CO::Ne => builder.build_float_compare(FloatPredicate::ONE, lhs, rhs, "ne"),
                CO::Lt => builder.build_float_compare(FloatPredicate::OLT, lhs, rhs, "lt"),
                CO::Le => builder.build_float_compare(FloatPredicate::OLE, lhs, rhs, "le"),
                CO::Gt => builder.build_float_compare(FloatPredicate::OGT, lhs, rhs, "gt"),
                CO::Ge => builder.build_float_compare(FloatPredicate::OGE, lhs, rhs, "ge"),
            };

            Value {
                ty: IntrinsicValueType::Bool,
                val: result.as_basic_value_enum(),
            }
        }
        _ => panic!(),
    }
}

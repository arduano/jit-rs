use inkwell::{values::BasicValueEnum, FloatPredicate, IntPredicate};

use crate::mir::{
    MirBinaryOp, MirIntrinsicBinaryOp, MirIntrinsicUnaryOp, MirIntrinsicVectorBinaryOp, MirType,
    MirUnaryOp, MirVectorBinaryOp,
};

use super::FunctionInsertContext;

pub fn codegen_binary_expr<'ctx>(
    op: &MirBinaryOp,
    ctx: &mut FunctionInsertContext<'ctx, '_>,
) -> Option<BasicValueEnum<'ctx>> {
    let lhs = ctx.write_expression(&op.lhs).unwrap();
    let rhs = ctx.write_expression(&op.rhs).unwrap();

    let builder = &ctx.module.builder;

    match &op.op {
        MirIntrinsicBinaryOp::IntAdd => Some(
            builder
                .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "add")
                .into(),
        ),
        MirIntrinsicBinaryOp::IntSub => Some(
            builder
                .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "sub")
                .into(),
        ),
        MirIntrinsicBinaryOp::IntMul => Some(
            builder
                .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "mul")
                .into(),
        ),
        MirIntrinsicBinaryOp::IntDiv => Some(
            builder
                .build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "div")
                .into(),
        ),
        MirIntrinsicBinaryOp::UIntDiv => Some(
            builder
                .build_int_unsigned_div(lhs.into_int_value(), rhs.into_int_value(), "div")
                .into(),
        ),
        MirIntrinsicBinaryOp::IntRem => Some(
            builder
                .build_int_signed_rem(lhs.into_int_value(), rhs.into_int_value(), "rem")
                .into(),
        ),
        MirIntrinsicBinaryOp::UIntRem => Some(
            builder
                .build_int_unsigned_rem(lhs.into_int_value(), rhs.into_int_value(), "rem")
                .into(),
        ),
        MirIntrinsicBinaryOp::IntShl => Some(
            builder
                .build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "shl")
                .into(),
        ),
        MirIntrinsicBinaryOp::IntShr => Some(
            builder
                .build_right_shift(lhs.into_int_value(), rhs.into_int_value(), true, "shr")
                .into(),
        ),
        MirIntrinsicBinaryOp::UIntShr => Some(
            builder
                .build_right_shift(lhs.into_int_value(), rhs.into_int_value(), false, "shr")
                .into(),
        ),
        MirIntrinsicBinaryOp::IntAnd => Some(
            builder
                .build_and(lhs.into_int_value(), rhs.into_int_value(), "and")
                .into(),
        ),
        MirIntrinsicBinaryOp::IntOr => Some(
            builder
                .build_or(lhs.into_int_value(), rhs.into_int_value(), "or")
                .into(),
        ),
        MirIntrinsicBinaryOp::IntXor => Some(
            builder
                .build_xor(lhs.into_int_value(), rhs.into_int_value(), "xor")
                .into(),
        ),
        MirIntrinsicBinaryOp::IntEq => Some(
            builder
                .build_int_compare(
                    IntPredicate::EQ,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "eq",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::IntNeq => Some(
            builder
                .build_int_compare(
                    IntPredicate::NE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "ne",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::IntLt => Some(
            builder
                .build_int_compare(
                    IntPredicate::SLT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "lt",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::IntLte => Some(
            builder
                .build_int_compare(
                    IntPredicate::SLE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "le",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::IntGt => Some(
            builder
                .build_int_compare(
                    IntPredicate::SGT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "gt",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::IntGte => Some(
            builder
                .build_int_compare(
                    IntPredicate::SGE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "ge",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::UIntLt => Some(
            builder
                .build_int_compare(
                    IntPredicate::ULT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "lt",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::UIntLte => Some(
            builder
                .build_int_compare(
                    IntPredicate::ULE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "le",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::UIntGt => Some(
            builder
                .build_int_compare(
                    IntPredicate::UGT,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "gt",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::UIntGte => Some(
            builder
                .build_int_compare(
                    IntPredicate::UGE,
                    lhs.into_int_value(),
                    rhs.into_int_value(),
                    "ge",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatAdd => Some(
            builder
                .build_float_add(lhs.into_float_value(), rhs.into_float_value(), "add")
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatSub => Some(
            builder
                .build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "sub")
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatMul => Some(
            builder
                .build_float_mul(lhs.into_float_value(), rhs.into_float_value(), "mul")
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatDiv => Some(
            builder
                .build_float_div(lhs.into_float_value(), rhs.into_float_value(), "div")
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatRem => Some(
            builder
                .build_float_rem(lhs.into_float_value(), rhs.into_float_value(), "rem")
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatEq => Some(
            builder
                .build_float_compare(
                    FloatPredicate::OEQ,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "eq",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatNeq => Some(
            builder
                .build_float_compare(
                    FloatPredicate::ONE,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "ne",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatLt => Some(
            builder
                .build_float_compare(
                    FloatPredicate::OLT,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "lt",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatLte => Some(
            builder
                .build_float_compare(
                    FloatPredicate::OLE,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "le",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatGt => Some(
            builder
                .build_float_compare(
                    FloatPredicate::OGT,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "gt",
                )
                .into(),
        ),
        MirIntrinsicBinaryOp::FloatGte => Some(
            builder
                .build_float_compare(
                    FloatPredicate::OGE,
                    lhs.into_float_value(),
                    rhs.into_float_value(),
                    "ge",
                )
                .into(),
        ),
    }
}

pub fn codegen_vector_binary_expr<'ctx>(
    op: &MirVectorBinaryOp,
    ctx: &mut FunctionInsertContext<'ctx, '_>,
) -> Option<BasicValueEnum<'ctx>> {
    let lhs = ctx.write_expression(&op.lhs).unwrap().into_vector_value();
    let rhs = ctx.write_expression(&op.rhs).unwrap().into_vector_value();

    match &op.op {
        MirIntrinsicVectorBinaryOp::IntAdd => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "add", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntSub => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "sub", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntMul => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "mul", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntDiv => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "sdiv", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::UIntDiv => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "udiv", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntRem => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "srem", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::UIntRem => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "urem", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntShl => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "shl", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntShr => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "ashr", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::UIntShr => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "lshr", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntAnd => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "and", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntOr => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "or", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntXor => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "xor", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntMax => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "smax", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntMin => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "smin", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntUMax => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "umax", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::IntUMin => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "umin", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::FloatAdd => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "fadd", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::FloatSub => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "fsub", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::FloatMul => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "fmul", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::FloatDiv => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "fdiv", op.scalar_ty, op.width))
        }
        MirIntrinsicVectorBinaryOp::FloatRem => {
            Some(ctx.call_vector_arithmetic_intrinsic(lhs, rhs, "frem", op.scalar_ty, op.width))
        }
    }
}

pub fn codegen_unary_expr<'ctx>(
    op: &MirUnaryOp,
    expr_ty: &MirType,
    ctx: &mut FunctionInsertContext<'ctx, '_>,
) -> Option<BasicValueEnum<'ctx>> {
    let operand = ctx.write_expression(&op.operand).unwrap();

    let builder = &ctx.module.builder;

    match &op.op {
        MirIntrinsicUnaryOp::IntNeg => Some(
            builder
                .build_int_neg(operand.into_int_value(), "neg")
                .into(),
        ),
        MirIntrinsicUnaryOp::FloatNeg => Some(
            builder
                .build_float_neg(operand.into_float_value(), "neg")
                .into(),
        ),
        MirIntrinsicUnaryOp::BoolNot => {
            Some(builder.build_not(operand.into_int_value(), "not").into())
        }
        MirIntrinsicUnaryOp::PointerDeref => {
            let ptr = operand.into_pointer_value();
            let pointee_ty = ctx.get_type(expr_ty);

            let value = builder.build_load(pointee_ty, ptr, "deref");

            Some(value.into())
        }
    }
}

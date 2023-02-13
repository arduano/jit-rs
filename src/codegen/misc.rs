use inkwell::{
    types::VectorType,
    values::{BasicValueEnum, VectorValue},
};

use crate::common::NumberKind;

use super::FunctionInsertContext;

pub fn codegen_extend_into_vector<'ctx>(
    ctx: &mut FunctionInsertContext<'ctx, '_>,
    value: BasicValueEnum<'ctx>,
    scalar_ty: NumberKind,
    width: usize,
) -> VectorValue<'ctx> {
    let vector_ty = ctx.module.get_vector_type(&scalar_ty, width as u32);
    let zeroed = vector_ty.const_zero();

    let i32_ty = ctx.module.context.i32_type();

    let mut vec = zeroed;
    for i in 0..width {
        let idx = i32_ty.const_int(i as u64, false);
        vec = ctx
            .module
            .builder
            .build_insert_element(vec, value, idx, "spread");
    }

    vec
}

pub fn codegen_get_true_vector<'ctx, 'a>(
    width: usize,
    ctx: &mut FunctionInsertContext<'ctx, 'a>,
) -> VectorValue<'ctx> {
    let mut values = Vec::with_capacity(width);
    let value = ctx.module.context.bool_type().const_int(1 as u64, false);
    values.resize_with(width, || value.clone());
    let vector = VectorType::const_vector(&values);
    vector
}

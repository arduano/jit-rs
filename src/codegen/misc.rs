use inkwell::{
    types::VectorType,
    values::{BasicValueEnum, VectorValue},
};

use super::FunctionInsertContext;

pub fn codegen_extend_into_vector<'ctx>(
    _ctx: &mut FunctionInsertContext,
    value: BasicValueEnum<'ctx>,
    width: usize,
) -> VectorValue<'ctx> {
    let mut values = Vec::with_capacity(width);
    values.resize_with(width, || value.clone());
    let vector = VectorType::const_vector(&values);
    vector
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

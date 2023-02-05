use std::path::PathBuf;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::BasicType,
    values::{BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, IntPredicate, OptimizationLevel,
};

use crate::mir::*;

pub struct LlvmCodegen {
    context: Context,
}

impl LlvmCodegen {
    pub fn new() -> Self {
        Self {
            context: Context::create(),
        }
    }

    pub fn insert_module(&mut self, module: &MirModule) -> LlvmCodegenModule {
        LlvmCodegenModule::new(&self.context, "test", module)
    }
}

pub struct LlvmCodegenModule<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> LlvmCodegenModule<'ctx> {
    pub fn execution_engine(&self) -> ExecutionEngine<'ctx> {
        self.module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap()
    }

    pub fn print_functions(&self) {
        for function in self.module.get_functions() {
            function.print_to_stderr();
        }
    }

    pub fn optimize(&self) {
        let pass_manager = PassManager::<Module>::create(());

        pass_manager.add_function_attrs_pass();
        pass_manager.add_demote_memory_to_register_pass();
        pass_manager.add_type_based_alias_analysis_pass();
        pass_manager.add_basic_alias_analysis_pass();
        pass_manager.add_correlated_value_propagation_pass();
        pass_manager.add_licm_pass();
        pass_manager.add_aggressive_dce_pass();
        pass_manager.add_aggressive_inst_combiner_pass();
        pass_manager.add_bit_tracking_dce_pass();
        pass_manager.add_reassociate_pass();
        pass_manager.add_gvn_pass();
        pass_manager.add_tail_call_elimination_pass();
        pass_manager.add_function_inlining_pass();
        pass_manager.add_always_inliner_pass();
        pass_manager.add_loop_deletion_pass();
        pass_manager.add_loop_idiom_pass();
        pass_manager.add_loop_vectorize_pass();
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_instruction_simplify_pass();
        pass_manager.add_loop_rotate_pass();
        pass_manager.add_loop_unroll_pass();
        pass_manager.add_lower_switch_pass();
        pass_manager.add_ind_var_simplify_pass();
        pass_manager.add_instruction_simplify_pass();
        pass_manager.add_global_optimizer_pass();
        pass_manager.add_dead_arg_elimination_pass();
        pass_manager.add_dead_store_elimination_pass();
        pass_manager.add_global_dce_pass();
        pass_manager.add_aggressive_dce_pass();
        pass_manager.add_bit_tracking_dce_pass();
        pass_manager.add_cfg_simplification_pass();
        pass_manager.add_gvn_pass();
        pass_manager.add_licm_pass();
        pass_manager.add_gvn_pass();
        pass_manager.add_cfg_simplification_pass();
        pass_manager.add_correlated_value_propagation_pass();

        pass_manager.run_on(&self.module);
    }

    pub fn write_to_file(&self) {
        Target::initialize_native(&InitializationConfig {
            base: true,
            asm_parser: false,
            asm_printer: true,
            disassembler: false,
            info: false,
            machine_code: false,
        })
        .unwrap();

        let target = Target::from_name("x86-64").unwrap();
        let target_machine = target
            .create_target_machine(
                &TargetTriple::create("x86_64-pc-linux-gnu"),
                "x86-64",
                "+avx2,+sse2,+sse4.1,+sse4.2",
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        let path = PathBuf::from("/home/arduano/programming/jit-rs/test.o");

        target_machine
            .write_to_file(&self.module, FileType::Object, &path)
            .unwrap();
    }

    fn new(context: &'ctx Context, name: &str, mir: &MirModule) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();

        let module = Self {
            context,
            module,
            builder,
        };

        let mut functions = Vec::new();
        for func in mir.functions.iter() {
            functions.push(module.declare_function(&func.decl));
        }

        for (func, fn_val) in mir.functions.iter().zip(functions) {
            module.implement_function(func, fn_val);
        }

        module
    }

    fn get_type(&self, ty: &MirType) -> inkwell::types::BasicTypeEnum<'ctx> {
        use MirIntrinsicType as IT;

        match &ty.kind {
            MirTypeKind::Intrinsic(ty) => match ty {
                IT::I8 => self.context.i8_type().into(),
                IT::I16 => self.context.i16_type().into(),
                IT::I32 => self.context.i32_type().into(),
                IT::I64 => self.context.i64_type().into(),
                IT::U8 => self.context.i8_type().into(),
                IT::U16 => self.context.i16_type().into(),
                IT::U32 => self.context.i32_type().into(),
                IT::U64 => self.context.i64_type().into(),
                IT::F32 => self.context.f32_type().into(),
                IT::F64 => self.context.f64_type().into(),
                IT::Bool => self.context.bool_type().into(),
                IT::Void => panic!("Unexpected void type"),
                IT::Never => panic!("Unexpected never type"),
                IT::Ptr(ty) => self.get_type(ty).ptr_type(AddressSpace::default()).into(),
            },
        }
    }

    fn get_fn_type<'b>(
        &self,
        ty: &MirType,
        param_types: impl Iterator<Item = &'b MirType>,
    ) -> inkwell::types::FunctionType<'ctx> {
        use MirIntrinsicType as IT;

        let args = param_types
            .map(|arg| self.get_type(&arg).into())
            .collect::<Vec<_>>();

        match ty.kind {
            MirTypeKind::Intrinsic(IT::Void) | MirTypeKind::Intrinsic(IT::Never) => {
                self.context.void_type().fn_type(&args, false)
            }

            _ => self.get_type(&ty).fn_type(&args, false),
        }
    }

    fn declare_function(&self, function: &MirFunctionDeclaration) -> FunctionValue<'ctx> {
        let param_types = function.args.iter().map(|param| &param.ty);

        let function_type = self.get_fn_type(&function.ret_type, param_types);

        let function = self
            .module
            .add_function(&function.name, function_type, None);

        function
    }

    fn implement_function(&self, function: &MirFunction, fn_value: FunctionValue<'ctx>) {
        FunctionInsertContext::implement(self, function, fn_value);
    }
}

pub struct FunctionInsertContext<'ctx, 'a> {
    module: &'a LlvmCodegenModule<'ctx>,
    variables: Vec<PointerValue<'ctx>>,
    blocks: Vec<BasicBlock<'ctx>>,
    fn_value: FunctionValue<'ctx>,
}

impl<'ctx: 'a, 'a> FunctionInsertContext<'ctx, 'a> {
    fn implement(
        module: &'a LlvmCodegenModule<'ctx>,
        function: &MirFunction,
        fn_value: FunctionValue<'ctx>,
    ) {
        let mut blocks = Vec::new();
        for _ in 0..function.blocks.len() {
            blocks.push(module.context.append_basic_block(fn_value, "block"));
        }

        module.builder.position_at_end(blocks[0]);

        let mut variables = Vec::new();
        for var in &function.variables {
            let ty = module.get_type(&var.ty);
            let alloca = module.builder.build_alloca(ty, "var");
            variables.push(alloca.into());
        }

        let mut ctx = Self {
            module,
            variables,
            blocks,
            fn_value,
        };

        for (i, block) in function.blocks.iter().enumerate() {
            module.builder.position_at_end(ctx.blocks[i]);
            ctx.write_block(block);
        }
    }

    fn get_type(&self, ty: &MirType) -> inkwell::types::BasicTypeEnum<'ctx> {
        self.module.get_type(ty)
    }

    fn write_block(&mut self, body: &MirBlock) {
        for statement in &body.statements {
            match &statement.kind {
                MirStatementKind::Return(expr) => {
                    let value = self.write_expression(&expr);
                    if let Some(value) = value {
                        self.module.builder.build_return(Some(&value));
                    } else {
                        self.module.builder.build_return(None);
                    }
                }
                MirStatementKind::PtrAssign(ass) => {
                    let value = self.write_expression(&ass.value).unwrap();
                    let ptr = self.write_expression(&ass.ptr).unwrap();
                    self.module
                        .builder
                        .build_store(ptr.into_pointer_value(), value);
                }
                MirStatementKind::Jump(jump) => {
                    let block = self.blocks[jump.index];
                    self.module.builder.build_unconditional_branch(block);
                }
                MirStatementKind::ConditionalJump(condjump) => {
                    let cond = self.write_expression(&condjump.condition);
                    let then_block = self.blocks[condjump.then_index];
                    let else_block = self.blocks[condjump.else_index];

                    self.module.builder.build_conditional_branch(
                        cond.unwrap().into_int_value(),
                        then_block,
                        else_block,
                    );
                }
            }
        }
    }

    fn write_expression(&mut self, expr: &MirExpression) -> Option<BasicValueEnum<'ctx>> {
        match &expr.kind {
            MirExpressionKind::ReadArg(arg) => {
                let arg = self.fn_value.get_nth_param(arg.index as u32).unwrap();
                Some(arg)
            }
            MirExpressionKind::GetVariablePtr(read_var) => {
                let variable = self.variables[read_var.var.index];
                Some(variable.into())
            }
            MirExpressionKind::Literal(lit) => {
                let ctx = &self.module.context;

                Some(match lit {
                    MirLiteral::Bool(val) => ctx.bool_type().const_int(*val as u64, false).into(),
                    MirLiteral::U8(val) => ctx.i8_type().const_int(*val as u64, false).into(),
                    MirLiteral::U16(val) => ctx.i16_type().const_int(*val as u64, false).into(),
                    MirLiteral::U32(val) => ctx.i32_type().const_int(*val as u64, false).into(),
                    MirLiteral::U64(val) => ctx.i64_type().const_int(*val as u64, false).into(),
                    MirLiteral::I8(val) => ctx.i8_type().const_int(*val as u64, true).into(),
                    MirLiteral::I16(val) => ctx.i16_type().const_int(*val as u64, true).into(),
                    MirLiteral::I32(val) => ctx.i32_type().const_int(*val as u64, true).into(),
                    MirLiteral::I64(val) => ctx.i64_type().const_int(*val as u64, true).into(),
                    MirLiteral::F32(val) => ctx.f32_type().const_float(*val as f64).into(),
                    MirLiteral::F64(val) => ctx.f64_type().const_float(*val).into(),
                })
            }
            MirExpressionKind::NoValue => None,
            MirExpressionKind::BinaryOp(op) => {
                let lhs = self.write_expression(&op.lhs).unwrap();
                let rhs = self.write_expression(&op.rhs).unwrap();

                let builder = &self.module.builder;

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
                            .build_int_unsigned_div(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                "div",
                            )
                            .into(),
                    ),
                    MirIntrinsicBinaryOp::IntRem => Some(
                        builder
                            .build_int_signed_rem(lhs.into_int_value(), rhs.into_int_value(), "rem")
                            .into(),
                    ),
                    MirIntrinsicBinaryOp::UIntRem => Some(
                        builder
                            .build_int_unsigned_rem(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                "rem",
                            )
                            .into(),
                    ),
                    MirIntrinsicBinaryOp::IntShl => Some(
                        builder
                            .build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "shl")
                            .into(),
                    ),
                    MirIntrinsicBinaryOp::IntShr => Some(
                        builder
                            .build_right_shift(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                true,
                                "shr",
                            )
                            .into(),
                    ),
                    MirIntrinsicBinaryOp::UIntShr => Some(
                        builder
                            .build_right_shift(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                false,
                                "shr",
                            )
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
                    MirIntrinsicBinaryOp::FloatAdd => todo!(),
                    MirIntrinsicBinaryOp::FloatSub => todo!(),
                    MirIntrinsicBinaryOp::FloatMul => todo!(),
                    MirIntrinsicBinaryOp::FloatDiv => todo!(),
                    MirIntrinsicBinaryOp::FloatRem => todo!(),
                    MirIntrinsicBinaryOp::FloatEq => todo!(),
                    MirIntrinsicBinaryOp::FloatNeq => todo!(),
                    MirIntrinsicBinaryOp::FloatLt => todo!(),
                    MirIntrinsicBinaryOp::FloatLte => todo!(),
                    MirIntrinsicBinaryOp::FloatGt => todo!(),
                    MirIntrinsicBinaryOp::FloatGte => todo!(),
                }
            }
            MirExpressionKind::IndexPtr(index) => {
                let value = self.write_expression(&index.value).unwrap();
                let index = self.write_expression(&index.index).unwrap();

                let value = value.into_pointer_value();
                let index = index.into_int_value();

                let pointee_ty = self.get_type(&expr.ty);

                let ptr = unsafe {
                    self.module
                        .builder
                        .build_gep(pointee_ty, value, &[index], "index")
                };

                Some(ptr.into())
            }
            MirExpressionKind::PtrDeref(deref) => {
                let ptr = self.write_expression(&deref.ptr).unwrap();
                let ptr = ptr.into_pointer_value();

                let pointee_ty = self.get_type(&expr.ty);

                let value = self.module.builder.build_load(pointee_ty, ptr, "deref");

                Some(value.into())
            }
        }
    }
}

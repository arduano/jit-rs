use std::{collections::HashSet, path::PathBuf};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, IntType, VectorType},
    values::{BasicValueEnum, FunctionValue, PointerValue, VectorValue},
    AddressSpace, OptimizationLevel,
};

use crate::{
    common::{FloatBits, IntBits, NumberKind},
    mir::*,
};

use self::intrinsics::{codegen_binary_expr, codegen_unary_expr, codegen_vector_binary_expr};

mod intrinsics;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
enum SizeBits {
    Bits32,
    Bits64,
}

pub struct LlvmCodegenModule<'ctx> {
    size_bits: SizeBits,
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    llvm_intrinsics_added: HashSet<String>,
}

impl<'ctx> LlvmCodegenModule<'ctx> {
    pub fn execution_engine(&self) -> ExecutionEngine<'ctx> {
        // Calling this is required to make sure the JIT doesn't get optimized away
        // during LTO. Otherwise we'll have a lot of errors.
        ExecutionEngine::link_in_mc_jit();

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
        pass_manager.add_ind_var_simplify_pass();
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

        // Set at compile time because this is a jit that doesn't cross-compile.
        #[cfg(target_pointer_width = "64")]
        let size_bits = SizeBits::Bits64;
        #[cfg(target_pointer_width = "32")]
        let size_bits = SizeBits::Bits32;

        let mut module = Self {
            context,
            module,
            builder,
            size_bits,
            llvm_intrinsics_added: HashSet::new(),
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

    fn get_int_type(&self, bits: &IntBits) -> IntType<'ctx> {
        match &bits {
            IntBits::Bits8 => self.context.i8_type(),
            IntBits::Bits16 => self.context.i16_type(),
            IntBits::Bits32 => self.context.i32_type(),
            IntBits::Bits64 => self.context.i64_type(),
            IntBits::BitsSize => match self.size_bits {
                SizeBits::Bits32 => self.context.i32_type(),
                SizeBits::Bits64 => self.context.i64_type(),
            },
        }
    }

    fn get_float_type(&self, bits: &FloatBits) -> FloatType<'ctx> {
        match &bits {
            FloatBits::Bits32 => self.context.f32_type(),
            FloatBits::Bits64 => self.context.f64_type(),
        }
    }

    fn get_vector_type(&self, ty: &NumberKind, width: u32) -> VectorType<'ctx> {
        match &ty {
            NumberKind::UnsignedInt(bits) => self.get_int_type(bits).vec_type(width),
            NumberKind::SignedInt(bits) => self.get_int_type(bits).vec_type(width),
            NumberKind::Float(bits) => self.get_float_type(bits).vec_type(width),
        }
    }

    fn get_type(&self, ty: &MirType) -> BasicTypeEnum<'ctx> {
        match &ty {
            MirType::Num(ty) => match ty {
                NumberKind::UnsignedInt(bits) => self.get_int_type(bits).into(),
                NumberKind::SignedInt(bits) => self.get_int_type(bits).into(),
                NumberKind::Float(bits) => self.get_float_type(bits).into(),
            },

            MirType::Bool => self.context.bool_type().into(),
            MirType::Void => panic!("Unexpected void type"),
            MirType::Never => panic!("Unexpected never type"),
            MirType::Ptr(ty) => self.get_type(ty).ptr_type(AddressSpace::default()).into(),
            MirType::ConstArray(ty, size) => self.get_type(ty).array_type(*size).into(),
            MirType::Vector(ty, width) => self.get_vector_type(ty, *width).into(),
        }
    }

    fn get_fn_type<'b>(
        &self,
        ty: &MirType,
        param_types: impl Iterator<Item = &'b MirType>,
    ) -> inkwell::types::FunctionType<'ctx> {
        let args = param_types
            .map(|arg| self.get_type(&arg).into())
            .collect::<Vec<_>>();

        match ty {
            MirType::Void | MirType::Never => self.context.void_type().fn_type(&args, false),

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

    fn implement_function(&mut self, function: &MirFunction, fn_value: FunctionValue<'ctx>) {
        FunctionInsertContext::implement(self, function, fn_value);
    }

    fn insert_intrinsic(
        &mut self,
        name: String,
        ret: BasicTypeEnum<'ctx>,
        args: &[BasicMetadataTypeEnum<'ctx>],
    ) -> FunctionValue<'ctx> {
        if self.llvm_intrinsics_added.contains(&name) {
            return self.module.get_function(&name).unwrap();
        }

        self.llvm_intrinsics_added.insert(name.clone());

        let fn_type = ret.fn_type(args, false);
        self.module.add_function(&name, fn_type, None)
    }
}

pub struct FunctionInsertContext<'ctx, 'a> {
    module: &'a mut LlvmCodegenModule<'ctx>,
    variables: Vec<PointerValue<'ctx>>,
    blocks: Vec<BasicBlock<'ctx>>,
    fn_value: FunctionValue<'ctx>,
}

impl<'ctx: 'a, 'a> FunctionInsertContext<'ctx, 'a> {
    fn implement(
        module: &'a mut LlvmCodegenModule<'ctx>,
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
            ctx.module.builder.position_at_end(ctx.blocks[i]);
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
                    if let Some(expr) = expr {
                        let value = self.write_expression(&expr);
                        if let Some(value) = value {
                            self.module.builder.build_return(Some(&value));
                        } else {
                            self.module.builder.build_return(None);
                        }
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

                    MirLiteral::USize(val) => match self.module.size_bits {
                        SizeBits::Bits32 => ctx.i32_type().const_int(*val as u64, false).into(),
                        SizeBits::Bits64 => ctx.i64_type().const_int(*val as u64, false).into(),
                    },
                    MirLiteral::ISize(val) => match self.module.size_bits {
                        SizeBits::Bits32 => ctx.i32_type().const_int(*val as u64, false).into(),
                        SizeBits::Bits64 => ctx.i64_type().const_int(*val as u64, false).into(),
                    },
                })
            }
            MirExpressionKind::NoValue => None,
            MirExpressionKind::BinaryOp(op) => codegen_binary_expr(op, self),
            MirExpressionKind::VectorBinaryOp(op) => codegen_vector_binary_expr(op, self),
            MirExpressionKind::UnaryOp(op) => codegen_unary_expr(op, &expr.ty, self),
            MirExpressionKind::IndexPtr(index_ptr) => {
                let value = self.write_expression(&index_ptr.value).unwrap();
                let index = self.write_expression(&index_ptr.index).unwrap();

                let value = value.into_pointer_value();
                let index = index.into_int_value();

                let pointee_ty = self.get_type(&index_ptr.index_ty);

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

                let pointee_ty = self.get_type(&deref.underlying_ty);

                let value = self.module.builder.build_load(pointee_ty, ptr, "deref");

                Some(value.into())
            }
            MirExpressionKind::VectorExtend(extend) => {
                let value = self.write_expression(&extend.unit).unwrap();

                let mut values = Vec::with_capacity(extend.width as usize);
                values.resize_with(extend.width as usize, || value.clone());
                let vector = VectorType::const_vector(&values);

                Some(vector.into())
            }
        }
    }

    fn call_vector_arithmetic_intrinsic(
        &mut self,
        left: VectorValue<'ctx>,
        right: VectorValue<'ctx>,
        name: &'static str,
        ty: NumberKind,
        width: u32,
    ) -> BasicValueEnum<'ctx> {
        let vector_ty = self.module.get_vector_type(&ty, width);
        let mask_ty = self.module.context.bool_type().vec_type(width);
        let i32_ty = self.module.context.i32_type();

        let is_invalid_ty = match ty {
            NumberKind::SignedInt(IntBits::BitsSize)
            | NumberKind::UnsignedInt(IntBits::BitsSize) => true,
            _ => false,
        };

        if is_invalid_ty {
            panic!("Invalid type for vector arithmetic intrinsic");
        }

        let intrinsic_name = format!("llvm.vp.{}.v{}{}", name, width, ty);

        let params = vec![
            vector_ty.into(),
            vector_ty.into(),
            mask_ty.into(),
            i32_ty.into(),
        ];

        let func = self
            .module
            .insert_intrinsic(intrinsic_name, vector_ty.into(), &params);

        let mask = mask_ty.const_zero();
        let size = i32_ty.const_int(width as u64, false);

        let args = vec![left.into(), right.into(), mask.into(), size.into()];
        let result = self.module.builder.build_call(func, &args, &name);

        result.try_as_basic_value().left().unwrap()
    }
}

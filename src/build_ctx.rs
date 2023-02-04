use std::{collections::HashMap, path::PathBuf};

use inkwell::{
    attributes::{self, Attribute, AttributeLoc},
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::{Linkage, Module},
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, PointerValue},
    OptimizationLevel,
};

use crate::{
    syntax::{self, BodyStatement, BodyStatementKind, Expression, Function},
    types::IntrinsicValueType,
};

use self::ops::{build_binary_math_op, build_comparison_op, build_constant_value};

mod ops;

pub struct VariableType<'a> {
    ty: IntrinsicValueType,
    llvm_ty: BasicTypeEnum<'a>,
    ptr: PointerValue<'a>,
}

pub struct Value<'a> {
    ty: IntrinsicValueType,
    val: BasicValueEnum<'a>,
}

pub struct GlobalCtx {
    context: Context,
}

impl GlobalCtx {
    pub fn new(context: Context) -> Self {
        GlobalCtx { context }
    }

    fn get_type<'a>(&'a self, type_: &syntax::Type) -> BasicTypeEnum<'a> {
        use IntrinsicValueType as I;
        match type_.ty {
            I::Bool => self.context.bool_type().as_basic_type_enum(),
            I::U8 => self.context.i8_type().as_basic_type_enum(),
            I::U16 => self.context.i16_type().as_basic_type_enum(),
            I::U32 => self.context.i32_type().as_basic_type_enum(),
            I::U64 => self.context.i64_type().as_basic_type_enum(),
            I::I8 => self.context.i8_type().as_basic_type_enum(),
            I::I16 => self.context.i16_type().as_basic_type_enum(),
            I::I32 => self.context.i32_type().as_basic_type_enum(),
            I::I64 => self.context.i64_type().as_basic_type_enum(),
            I::F32 => self.context.f32_type().as_basic_type_enum(),
            I::F64 => self.context.f64_type().as_basic_type_enum(),
            // _ => panic!("Unknown type: {}", type_.name), // FIXME: error handling
        }
    }

    pub fn new_module(&self, name: &str) -> ModuleCtx {
        ModuleCtx::new(self, name)
    }
}

pub struct ModuleCtx<'a> {
    global: &'a GlobalCtx,
    module: Module<'a>,
    builder: Builder<'a>,
}

impl<'a> ModuleCtx<'a> {
    fn new(global: &'a GlobalCtx, _name: &str) -> Self {
        let module = global.context.create_module("sum");
        let builder = global.context.create_builder();

        ModuleCtx {
            global,
            module,
            builder,
        }
    }

    pub fn optimize(&self) {
        let pass_manager = PassManager::<Module>::create(());

        pass_manager.add_type_based_alias_analysis_pass();
        pass_manager.add_basic_alias_analysis_pass();
        pass_manager.add_correlated_value_propagation_pass();
        pass_manager.add_licm_pass();
        pass_manager.add_aggressive_dce_pass();
        pass_manager.add_aggressive_inst_combiner_pass();
        pass_manager.add_bit_tracking_dce_pass();
        pass_manager.add_gvn_pass();
        pass_manager.add_tail_call_elimination_pass();
        pass_manager.add_function_inlining_pass();
        pass_manager.add_always_inliner_pass();
        pass_manager.add_ind_var_simplify_pass();
        pass_manager.add_global_optimizer_pass();
        pass_manager.add_dead_arg_elimination_pass();
        pass_manager.add_cfg_simplification_pass();
        pass_manager.add_dead_store_elimination_pass();
        pass_manager.add_instruction_combining_pass();
        pass_manager.add_instruction_simplify_pass();

        pass_manager.run_on(&self.module);
    }

    pub fn execution_engine(&self) -> ExecutionEngine<'a> {
        self.module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .unwrap()
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

    fn get_type(&self, type_: &syntax::Type) -> BasicTypeEnum<'a> {
        self.global.get_type(type_)
    }

    pub fn add_function(&self, function: &Function) {
        FunctionBuilder::build(self, function);
    }

    pub fn print_functions(&self) {
        for function in self.module.get_functions() {
            function.print_to_stderr();
        }
    }
}

pub struct FunctionBuilder<'a, 'ctx> {
    module: &'a ModuleCtx<'ctx>,
    variables: HashMap<String, VariableType<'ctx>>,
    return_type: Option<IntrinsicValueType>,
}

impl<'a, 'ctx> FunctionBuilder<'a, 'ctx> {
    fn build(module: &ModuleCtx<'a>, function: &Function) {
        let args = function
            .args
            .iter()
            .map(|arg| module.get_type(&arg.type_).into())
            .collect::<Vec<_>>();

        let fn_type = if let Some(ret_type) = &function.ret_type {
            module.get_type(ret_type).fn_type(&args, false)
        } else {
            module.global.context.void_type().fn_type(&args, false)
        };

        let fn_value = module.module.add_function(&function.name, fn_type, None);

        // fn_value.add_attribute(
        //     AttributeLoc::Function,
        //     module
        //         .global
        //         .context
        //         .create_enum_attribute(Attribute::get_named_enum_kind_id("alwaysinline"), 0),
        // );

        let entry = module.global.context.append_basic_block(fn_value, "entry");
        module.builder.position_at_end(entry);

        let variables = function
            .args
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let alloca = module
                    .builder
                    .build_alloca(module.get_type(&arg.type_), &arg.name);
                module
                    .builder
                    .build_store(alloca, fn_value.get_nth_param(i as u32).unwrap());

                let ty = VariableType {
                    llvm_ty: module.get_type(&arg.type_),
                    ty: arg.type_.ty,
                    ptr: alloca,
                };

                (arg.name.clone(), ty)
            })
            .collect::<HashMap<_, _>>();

        let mut builder = FunctionBuilder {
            module,
            variables,
            return_type: function.ret_type.as_ref().map(|ty| ty.ty),
        };

        let final_value = builder.process_statements(&function.body);

        if let Some(final_value) = final_value {
            module.builder.build_return(Some(&final_value.val));
        }

        fn_value.print_to_stderr();
    }

    fn process_statements(&mut self, statements: &[BodyStatement]) -> Option<Value<'a>> {
        for (i, statement) in statements.iter().enumerate() {
            if i == statements.len() - 1 {
                return self.process_statement(statement);
            } else {
                self.process_statement(statement);
            }
        }

        // Should never reach here
        None
    }

    fn process_statement(&mut self, statement: &BodyStatement) -> Option<Value<'a>> {
        match &statement.kind {
            BodyStatementKind::Return(expr) => {
                let val = self.process_expr(expr).unwrap();

                // self.module.builder.build_return(Some(&val));
                self.module.builder.build_return(Some(&val.val));
                None
            }
            BodyStatementKind::Expr(expr) => self.process_expr(expr),
        }
    }

    fn process_expr(&mut self, expr: &Expression) -> Option<Value<'a>> {
        match expr {
            Expression::ConstantValue(val) => {
                Some(build_constant_value(&self.module.global.context, val))
            }
            Expression::ReadVar(name) => {
                let var = self.variables.get(name).unwrap();
                let val = self.module.builder.build_load(var.llvm_ty, var.ptr, name);
                Some(Value { ty: var.ty, val })
            }
            Expression::SingleOp(_expr) => {
                todo!();
            }
            Expression::BinaryMathOp(expr) => {
                let lhs = self.process_expr(&expr.lhs).unwrap();
                let rhs = self.process_expr(&expr.rhs).unwrap();
                Some(build_binary_math_op(
                    &self.module.builder,
                    lhs,
                    rhs,
                    expr.op,
                ))
            }
            Expression::ComparisonOp(expr) => {
                let lhs = self.process_expr(&expr.lhs).unwrap();
                let rhs = self.process_expr(&expr.rhs).unwrap();
                Some(build_comparison_op(&self.module.builder, lhs, rhs, expr.op))
            }
            Expression::FnCall { name, args } => {
                let args = args
                    .iter()
                    .map(|arg| self.process_expr(arg).unwrap().val.into())
                    .collect::<Vec<_>>();

                let fn_value = self.module.module.get_function(name).unwrap();

                let val = self.module.builder.build_call(fn_value, &args, "call");

                Some(Value {
                    val: val.try_as_basic_value().left().unwrap(),
                    ty: IntrinsicValueType::U32, //FIXME: Analyze function return values
                })
            }
            Expression::If { cond, then, else_ } => {
                let cond = self.process_expr(cond).unwrap();
                let ctx = &self.module.global.context;
                let then_block = ctx.append_basic_block(
                    self.module
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "then",
                );
                let else_block = ctx.append_basic_block(
                    self.module
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "else",
                );
                let merge_block = ctx.append_basic_block(
                    self.module
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "merge",
                );

                self.module.builder.build_conditional_branch(
                    cond.val.into_int_value(),
                    then_block,
                    else_block,
                );

                self.module.builder.position_at_end(then_block);
                let val1 = self.process_statements(then);
                self.module.builder.build_unconditional_branch(merge_block);

                self.module.builder.position_at_end(else_block);
                let val2 = self.process_statements(else_);
                self.module.builder.build_unconditional_branch(merge_block);

                self.module.builder.position_at_end(merge_block);

                if val1.is_some() != val2.is_some() {
                    panic!("If statement must return the same type");
                }

                if val1.is_some() {
                    let val1 = val1.unwrap();
                    let val2 = val2.unwrap();
                    if val1.ty != val2.ty {
                        panic!("If statement must return the same type");
                    }

                    let phi = self.module.builder.build_phi(val1.val.get_type(), "if");
                    phi.add_incoming(&[(&val1.val, then_block), (&val2.val, else_block)]);
                    Some(Value {
                        ty: val1.ty,
                        val: phi.as_basic_value(),
                    })
                } else {
                    None
                }
            }
        }
    }
}

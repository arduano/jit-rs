use std::{collections::HashMap, path::PathBuf};

use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module,
    passes::PassManager,
    targets::{CodeModel, FileType, RelocMode, Target, TargetTriple},
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, PointerValue},
    OptimizationLevel,
};

use crate::{
    counter::Counter,
    syntax::{self, BodyStatement, BodyStatementKind, Expression, Function},
    types::IntrinsicValueType,
};

use self::ops::{build_binary_math_op, build_comparison_op};

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
    register_name_gen: Counter,
}

impl<'a> ModuleCtx<'a> {
    fn new(global: &'a GlobalCtx, _name: &str) -> Self {
        let module = global.context.create_module("sum");
        let builder = global.context.create_builder();

        Target::initialize_x86(&Default::default());
        let triple = TargetTriple::create("x86_64-pc-linux-gnu");
        module.set_triple(&triple);

        ModuleCtx {
            global,
            module,
            builder,
            register_name_gen: Counter::new(),
        }
    }

    pub fn optimize(&self) {
        let pass_manager = PassManager::<Module>::create(());

        pass_manager.add_ind_var_simplify_pass();
        pass_manager.add_global_optimizer_pass();
        pass_manager.add_dead_arg_elimination_pass();
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
        let target = Target::from_name("x86-64").unwrap();
        let target_machine = target
            .create_target_machine(
                &TargetTriple::create("x86_64-pc-linux-gnu"),
                "x86-64",
                "+avx2",
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
}

impl<'a, 'ctx> FunctionBuilder<'a, 'ctx> {
    fn build(module: &ModuleCtx<'a>, function: &Function) {
        let args = function
            .args
            .iter()
            .map(|arg| module.get_type(&arg.type_).into())
            .collect::<Vec<_>>();
        let fn_type = module.get_type(&function.ret_type).fn_type(&args, false);
        let fn_value = module.module.add_function(&function.name, fn_type, None);

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

        let builder = FunctionBuilder { module, variables };

        for statement in &function.body {
            builder.process_statement(statement);
        }

        fn_value.print_to_stderr();
    }

    fn process_statement(&self, statement: &BodyStatement) {
        match &statement.kind {
            BodyStatementKind::Return(expr) => {
                let val = self.process_expr(expr);
                self.module.builder.build_return(Some(&val.val));
            }
        }
    }

    fn process_expr(&self, expr: &Expression) -> Value<'a> {
        match expr {
            Expression::ReadVar(name) => {
                let var = self.variables.get(name).unwrap();
                let val = self.module.builder.build_load(var.llvm_ty, var.ptr, name);
                Value { ty: var.ty, val }
            }
            Expression::SingleOp(_expr) => {
                todo!();
            }
            Expression::BinaryMathOp(expr) => {
                let lhs = self.process_expr(&expr.lhs);
                let rhs = self.process_expr(&expr.rhs);
                build_binary_math_op(&self.module.builder, lhs, rhs, expr.op)
            }
            Expression::ComparisonOp(expr) => {
                let lhs = self.process_expr(&expr.lhs);
                let rhs = self.process_expr(&expr.rhs);
                build_comparison_op(&self.module.builder, lhs, rhs, expr.op)
            }
        }
    }
}

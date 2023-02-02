use inkwell::context::Context;

use syntax::Function;

use crate::build_ctx::GlobalCtx;
use crate::syntax::{BodyStatement, BodyStatementKind, Expression, Type, VariableDeclare};
use crate::types::IntrinsicValueType;

mod build_ctx;
mod counter;
mod syntax;
mod tokens;
mod types;

fn main() {
    let ctx = GlobalCtx::new(Context::create());
    let module = ctx.new_module("main");

    use Expression as E;

    let function = Function {
        name: "main".to_string(),
        args: vec![
            VariableDeclare::new("a", IntrinsicValueType::U32),
            VariableDeclare::new("b", IntrinsicValueType::U32),
            VariableDeclare::new("c", IntrinsicValueType::U32),
            VariableDeclare::new("d", IntrinsicValueType::U32),
        ],
        ret_type: Type::new(IntrinsicValueType::Bool),
        body: vec![BodyStatement {
            kind: BodyStatementKind::Return(E::lt(
                E::add(E::read_var("a"), E::mul(E::read_var("b"), E::read_var("c"))),
                E::read_var("d"),
            )),
        }],
    };

    module.add_function(&function);
    module.optimize();

    module.print_functions();

    module.write_to_file();

    let jit = module.execution_engine();
    let compiled = unsafe {
        jit.get_function::<unsafe extern "C" fn(u32, u32, u32, u32) -> bool>("main")
            .unwrap()
    };

    unsafe {
        dbg!(compiled.call(1, 10, 5, 50));
    }
}

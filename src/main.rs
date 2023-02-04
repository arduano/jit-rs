use inkwell::context::Context;

use macros::jit_quote;
use syntax::Function;

use crate::build_ctx::GlobalCtx;
use crate::codegen::LlvmCodegen;
use crate::mir::mir_parse_module;
use crate::syntax::{
    BodyStatement, BodyStatementKind, ConstantValue, Expression, Type, VariableDeclare,
};
use crate::types::IntrinsicValueType;

mod build_ctx;
mod codegen;
mod macro_builder;
mod mir;
mod syntax;
mod tokens;
mod tree_parser;
mod types;

use macro_builder::*;
use tree_parser::*;

fn main() {
    let tokens = jit_quote! {
        pub fn test(arg: u32) -> u32 {
            let test = arg;
            return if arg < 10u32 {
                if arg > 5u32 {
                    51u32
                } else {
                    10u32
                }
            } else {
                50u32
            }
        }
    };

    let tree = parse_tokens_to_tree(&tokens);

    println!("{:#?}", &tokens);
    println!("{:#?}", &tree);

    let module = mir_parse_module(&tree.unwrap()).unwrap();
    dbg!(&module);

    let mut ctx = LlvmCodegen::new();
    let module = ctx.insert_module(&module);

    dbg!("fns");
    module.print_functions();
    dbg!("optimizing");
    module.optimize();
    dbg!("optimized");
    module.print_functions();

    dbg!("writing to file");
    module.write_to_file();

    dbg!("running");

    let engine = module.execution_engine();

    let compiled = unsafe {
        engine
            .get_function::<unsafe extern "C" fn(u32) -> u32>("test")
            .unwrap()
    };

    println!("Result: {}", unsafe { compiled.call(1) });

    return;
}

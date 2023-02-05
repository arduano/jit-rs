use jit_rs::macro_builder::*;
use macros::jit_quote;
use std::hint::black_box;

use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;
use jit_rs::codegen::LlvmCodegen;
use jit_rs::mir::mir_parse_module;
use jit_rs::tree_parser::parse_tokens_to_tree;

#[inline(never)]
fn test_rs(arg: u32) -> u32 {
    let mut i = 0u32;
    let mut x = 0u32;
    while i < arg {
        x = (x % (i + 1u32)) ^ x ^ i;
        i = i + 1u32;
    }
    return x;
}

fn criterion_benchmark(c: &mut Criterion) {
    let tokens = jit_quote! {
        pub fn test(arg: u32) -> u32 {
            let i = 0u32;
            let x = 0u32;
            while i < arg {
                x = (x % (i + 1u32)) ^ x ^ i;
                i = i + 1u32;
            }
            return x;
        }
    };

    let tree = parse_tokens_to_tree(&tokens);

    let module = mir_parse_module(&tree.unwrap()).unwrap();

    let mut ctx = LlvmCodegen::new();
    let module = ctx.insert_module(&module);

    module.optimize();
    module.print_functions();

    let engine = module.execution_engine();

    let compiled = unsafe {
        engine
            .get_function::<unsafe extern "C" fn(u32) -> u32>("test")
            .unwrap()
    };

    let iters = 1000000u32;

    println!("Native Result: {}", test_rs(iters));
    println!("JIT Result: {}", unsafe { compiled.call(iters) });

    c.bench_function("native", |f| {
        f.iter(|| {
            test_rs(black_box(iters));
            test_rs(black_box(iters + 1));
            test_rs(black_box(iters + 2));
        })
    });

    c.bench_function("jit", |f| {
        f.iter(|| unsafe {
            compiled.call(iters);
            compiled.call(iters + 1);
            compiled.call(iters + 2);
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

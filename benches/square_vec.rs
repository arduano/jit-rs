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
fn square_fn(arg: &mut [i32]) {
    for val in arg {
        *val = *val * *val;
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    let tokens = jit_quote! {
        pub fn square_vec(arr: *i32, len: usize) {
            let i = 0usize;

            while len - i >= 8usize {
                let val = load_vec::<<i32; 8usize>>(&arr[i]);
                let val = val * val;
                store_vec::<<i32; 8usize>>(&arr[i], val);
                i = i + 8usize;
            }

            while i < len  {
                let val = arr[i];
                arr[i] = val * val;
                i = i + 1usize;
            }
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
            .get_function::<unsafe extern "C" fn(*mut i32, usize)>("square_vec")
            .unwrap()
    };

    let mut numbers = vec![11i32; 100000];

    c.bench_function("native", |f| {
        f.iter(|| {
            square_fn(black_box(&mut numbers));
        })
    });

    c.bench_function("jit", |f| {
        f.iter(|| unsafe {
            compiled.call(black_box(numbers.as_mut_ptr()), black_box(numbers.len()));
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

use std::arch::x86_64::__m256i;

use jit_rs::codegen::LlvmCodegen;
use jit_rs::macro_builder::*;
use jit_rs::mir::mir_parse_module;
use jit_rs::tree_parser::*;
use macros::jit_quote;

// #[inline(never)]
// fn test_rs(arg: *mut u32) -> u32 {
//     unsafe {
//         return *arg;
//     }
// }

fn main() {
    let tokens = jit_quote! {
        // pub fn test(arg: *[u32; 1u32]) -> [u32; 1u32] {
        //     arg[0u32] = 6666u32;
        //     arg[1u32] = 5555u32;
        //     return arg[1u32];
        // }
        // pub fn test(arg: *[u32; 2u32], index: usize) -> u32 {
        //     let arg = (*arg);
        //     return arg[index];
        // }
        // pub fn mod_ptr(arg: *u32) {
        //     *arg = 10u32;
        // }

        pub fn vec_op(arg: <u32; 8usize>) -> <u32; 8usize> {
            1u32 - arg
        }
    };

    println!("{}", &tokens);

    let tree = parse_tokens_to_tree(&tokens);

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

    let _arr = [1234u32, 2335u32];

    let result = unsafe {
        // let compiled = engine
        //     .get_function::<unsafe extern "C" fn(*const u32, usize) -> u32>("test")
        //     .unwrap();
        // compiled.call(arr.as_ptr(), 0)

        // let mut num = 0u32;
        // let num = &mut num as *mut u32;
        // let compiled = engine
        //     .get_function::<unsafe extern "C" fn(*mut u32)>("mod_ptr")
        //     .unwrap();
        // compiled.call(num);
        // *num

        let num_arr = [0u32, 1u32, 2u32, 3u32, 4u32, 5u32, 6u32, 7u32];
        let mut as_mm: __m256i = std::mem::transmute(num_arr);
        let num_arr = &mut as_mm as *mut __m256i;
        dbg!(num_arr);
        let compiled = engine
            .get_function::<unsafe extern "C" fn(__m256i) -> __m256i>("vec_op")
            .unwrap();
        as_mm = compiled.call(as_mm);
        let as_arr: [u32; 8] = std::mem::transmute(as_mm);
        as_arr
    };

    println!("Result: {:?}", result);

    // println!(
    //     "Native: {}",
    //     test_rs(std::hint::black_box(arr.as_mut_ptr()))
    // );
}

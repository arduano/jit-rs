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
        pub fn test(arg: *[u32; 2u32], index: u32) -> u32 {
            let arg = arg[0u32];
            return arg[index];
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

    let arr = [1234u32, 2335u32];

    let result = unsafe {
        let compiled = engine
            .get_function::<unsafe extern "C" fn(*const u32, u32) -> u32>("test")
            .unwrap();
        compiled.call(arr.as_ptr(), 0)
    };

    println!("Result: {}", result);

    // println!(
    //     "Native: {}",
    //     test_rs(std::hint::black_box(arr.as_mut_ptr()))
    // );
}

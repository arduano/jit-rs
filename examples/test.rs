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
        // pub fn vec_op(arg: <u32; 8usize>) -> <u32; 8usize> {
        //     arg * 3u32
        // }

        // pub fn add_together(target: *f32, with: *f32, length: usize) {
        //     1u32 - arg
        // }

        // pub fn cast(target: i32) -> f32 {
        //     target as f32
        // }

        pub fn cast_vec(arr: *i32) {
            let val = load_vec::<<i32; 8usize>>(arr);
            let val = val * val;
            store_vec::<<i32; 8usize>>(arr, val);
        }

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

        struct TestStruc {
            a: i32,
            b: i32,
        }

        pub fn structs(val: i32) -> i32 {
            let struc = TestStruc {
                a: val,
                b: 5i32
            };
            struc.b = 10i32;
            struc.a + struc.b
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

        // let num_arr = [0u32, 1u32, 2u32, 3u32, 4u32, 5u32, 6u32, 7u32];
        // let mut as_mm: __m256i = std::mem::transmute(num_arr);
        // let num_arr = &mut as_mm as *mut __m256i;
        // dbg!(num_arr);
        // let compiled = engine
        //     .get_function::<unsafe extern "C" fn(__m256i) -> __m256i>("vec_op")
        //     .unwrap();
        // as_mm = compiled.call(as_mm);
        // let as_arr: [u32; 8] = std::mem::transmute(as_mm);
        // as_arr

        // let compiled = engine
        //     .get_function::<unsafe extern "C" fn(i32) -> f32>("cast")
        //     .unwrap();
        // compiled.call(514)

        // let mut num_arr = [0i32, 1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32];
        // let ptr = &mut num_arr as *mut i32;
        // let compiled = engine
        //     .get_function::<unsafe extern "C" fn(*mut i32)>("cast_vec")
        //     .unwrap();
        // compiled.call(ptr);
        // num_arr

        // let mut num_arr = [
        //     0i32, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
        //     13, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 2, 2, 2,
        // ];
        // let ptr = &mut num_arr as *mut i32;
        // let compiled = engine
        //     .get_function::<unsafe extern "C" fn(*mut i32, usize)>("square_vec")
        //     .unwrap();
        // compiled.call(ptr, num_arr.len());
        // num_arr

        let compiled = engine
            .get_function::<unsafe extern "C" fn(i32) -> i32>("structs")
            .unwrap();
        compiled.call(514)
    };

    println!("Result: {:?}", result);

    // println!(
    //     "Native: {}",
    //     test_rs(std::hint::black_box(arr.as_mut_ptr()))
    // );
}

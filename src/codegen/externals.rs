use std::alloc::Layout;

use inkwell::{execution_engine::ExecutionEngine, module::Module, types::BasicType, AddressSpace};

pub fn module_append_external_functions(module: &Module) {
    let ctx = module.get_context();

    module.add_function(
        "memalloc",
        module
            .get_context()
            .i8_type()
            .ptr_type(AddressSpace::default())
            .fn_type(&[ctx.i32_type().into()], false),
        None,
    );

    module.add_function(
        "memdealloc",
        module.get_context().void_type().fn_type(
            &[
                ctx.i8_type().ptr_type(AddressSpace::default()).into(),
                ctx.i32_type().into(),
            ],
            false,
        ),
        None,
    );
}

pub fn get_alloc_fn<'ctx>(module: &Module<'ctx>) -> inkwell::values::FunctionValue<'ctx> {
    module
        .get_function("memalloc")
        .expect("memalloc function not found")
}

pub fn get_dealloc_fn<'ctx>(module: &Module<'ctx>) -> inkwell::values::FunctionValue<'ctx> {
    module
        .get_function("memdealloc")
        .expect("memdealloc function not found")
}

pub fn fill_execution_engine_externals(module: &Module, engine: &ExecutionEngine) {
    if let Some(alloc) = module.get_function("memalloc") {
        engine.add_global_mapping(&alloc, allocate as usize);
    }
    if let Some(drop) = module.get_function("memdealloc") {
        engine.add_global_mapping(&drop, deallocate as usize);
    }
}

unsafe fn allocate(size: usize) -> *mut u8 {
    std::alloc::alloc(Layout::from_size_align_unchecked(
        size,
        std::mem::size_of::<u64>(),
    ))
}

unsafe fn deallocate(ptr: *mut u8, size: usize) {
    std::alloc::dealloc(
        ptr,
        Layout::from_size_align_unchecked(size, std::mem::size_of::<u64>()),
    );
}

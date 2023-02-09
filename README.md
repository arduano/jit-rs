# Rust Jit

HIGHLY WORK IN PROGRESS

This is a DSL that's somewhere betwen Rust and C that's defined using Rust macros and is designed to JIT.

The use case is extremely high performance pipeline-style algorithms where different components can connect together but the configuration of the pipeline is not known at compile time.

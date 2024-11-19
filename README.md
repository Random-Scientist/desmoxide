## Desmoxide
Rusty desmos :)

### planned features/goals
 * ASM JIT backend (compile your Desmos expressions to assembly!) via llvm with the `inkwell` crate
 * SPIR-V backend for GPU-side execution of expressions (with support for uniform variables and performant dynamically sized lists with buffers)
 * Optimize IR with Equality Saturation and the `egglog` crate (Unfortunately, Egglog's Rust API is basically nonexistent at the moment, so this is on hold until they actually implement one)



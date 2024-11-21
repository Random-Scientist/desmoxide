## Desmoxide
Rusty desmos :)

This branch is a ground-up rewrite of desmoxide/master.

## content warning
This project is written in a Rust dialect affectionately referred to as "nerd Rust" (due to notable Desmos user Fadaaszhi).
As such, it features copious use of many silly, unsafe and advanced Rust features. 
The objective of usage of these features is usually an entirely unnecessary (slight) performance improvement, or just to have fun in general.

That said, the goal to expose simple interfaces, both to the user and from all of the overarching components of the project.

As an example of this philosophy, the `Frontend` struct has a very simple interface and zero lifetime or generic parameters.
Meanwhile, `Parser` (an implementation detail of `Frontend`) has a working type signature of `BrandedParser<'source, 'brand, T: GenericSpanIter<'source>>`
and uses an advanced technique to assert safety invariants at compile time using lifetimes and the type system.

### planned features/goals
 * simple (non-spaghetti) user-facing API
 * 1:1 support for Desmos expressions
 * autodiff-based automatic differentiation
 * JIT backend (compile your Desmos expressions to assembly!) via llvm with the `inkwell` crate
 * SPIR-V/wgpu backend for GPU-side execution of expressions (with support for uniform variables and performant dynamically sized lists with buffers)
 * Desmos-specific optimizations on the mid level IR provided by this crate (e.g. broadcast inlining)



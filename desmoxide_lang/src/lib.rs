use desmoxide_derive::inject_brand_lifetime;

#[warn(clippy::pedantic)]
/// Contains backends that lower the IR provided by `middle` to various targets
mod back;
/// Contains a frontend which lexes, parses, and lowers Desmos expressions to DesMIR
mod front;
/// Contains shared definitions and code for generating and working with DesMIR
mod middle;
/// Contains shared definitions of various useful structs
pub(crate) mod util;

#[inject_brand_lifetime('brand)]
pub struct Test {
    fiel1: File,
}
fn test() {
    let t = Test::new(File::open("").unwrap());
}
use std::fs::File;

#[inject_brand_lifetime('brand)]
pub(crate) struct Test22<T>(u32, T);

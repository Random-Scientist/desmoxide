use proc_macro::TokenStream;
use syn::parse_macro_input;

mod imp;

#[proc_macro_attribute]
pub fn inject_brand_lifetime(args: TokenStream, item: TokenStream) -> TokenStream {
    let out: TokenStream =
        imp::do_brand_attr(parse_macro_input!(args), parse_macro_input!(item)).into();
    out
}

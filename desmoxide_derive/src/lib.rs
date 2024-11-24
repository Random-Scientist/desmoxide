use proc_macro::TokenStream;
use syn::parse_macro_input;

mod imp;

#[proc_macro_attribute]
pub fn inject_brand_lifetime(args: TokenStream, item: TokenStream) -> TokenStream {
    let out: TokenStream =
        imp::derive_has_brand_lifetime_impl(parse_macro_input!(args), parse_macro_input!(item))
            .into();
    eprintln!("{}", &out.to_string());
    out
}

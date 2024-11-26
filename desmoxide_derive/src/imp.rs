use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    braced,
    parse::Parse,
    parse_quote,
    punctuated::Punctuated,
    token::{Brace, Colon, Comma, Const, Fn, Paren, SelfType},
    ExprStruct, Field, FieldValue, Fields, FieldsUnnamed, FnArg, GenericParam, Generics, Ident,
    Index, ItemStruct, Lifetime, LifetimeParam, Member, PatIdent, PatType, Signature, Token,
    Visibility,
};

pub struct Input {
    _mod_token: Token![mod],
    mod_name: Ident,
    _brace: Brace,
    structs: Vec<ItemStruct>,
}
impl Parse for Input {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mod_token = input.parse()?;
        let mod_name = input.parse()?;
        let items;
        let brace = braced!(items in input);
        let mut structs = Vec::new();
        while let Ok(item) = items.call(ItemStruct::parse) {
            structs.push(item);
        }
        Ok(Self {
            _mod_token: mod_token,
            mod_name,
            _brace: brace,
            structs,
        })
    }
}
pub(crate) fn do_brand_attr(brand: Lifetime, input: Input) -> TokenStream {
    let Input {
        mod_name, structs, ..
    } = input;
    let mut inners = Vec::new();
    let mut outers = Vec::new();
    for (inner, outer) in structs
        .into_iter()
        .map(|stru| derive_has_brand_lifetime_impl(brand.clone(), mod_name.clone(), stru))
    {
        inners.push(inner);
        outers.push(outer);
    }
    quote! {
        mod #mod_name {
            #(#inners)*
        }
        #(#outers)*
    }
}
// yes the code quality is awful
// yes it works (most of the time)
pub(crate) fn derive_has_brand_lifetime_impl(
    brand: Lifetime,
    mod_name: Ident,
    mut item: ItemStruct,
) -> (TokenStream, TokenStream) {
    let old_item = item.clone();
    let p = LifetimeParam::new(brand.clone());
    if !item.generics.lifetimes().any(|a| a == &p) {
        item.generics.params.push(syn::GenericParam::Lifetime(p));
    }
    for field in item.fields.iter_mut() {
        raise_visibility(&mut field.vis);
    }
    let mut f = Field {
        attrs: Vec::new(),
        vis: Visibility::Inherited,
        mutability: syn::FieldMutability::None,
        ident: None,
        colon_token: None,
        ty: syn::Type::Verbatim(parse_quote!(crate::util::branded::PhantomInvariant<#brand>)),
    };
    item.fields = match item.fields {
        syn::Fields::Named(mut fields_named) => {
            f.ident = Some(format_ident!("__{}", brand.ident.clone()));
            f.colon_token = Some(Colon(Span::call_site()));
            fields_named.named.push(f);
            syn::Fields::Named(fields_named)
        }
        syn::Fields::Unnamed(mut fields_unnamed) => {
            fields_unnamed.unnamed.push(f);
            syn::Fields::Unnamed(fields_unnamed)
        }
        syn::Fields::Unit => {
            let mut unnamed = Punctuated::new();
            unnamed.push(f);
            syn::Fields::Unnamed(FieldsUnnamed {
                paren_token: Paren(Span::call_site()),
                unnamed,
            })
        }
    };
    let mut generics_with_static = item.generics.clone();
    let mut generics_with_no_lt = item.generics.clone();
    let mut generics_with_brand_as_downcast = item.generics.clone();
    for (idx, i) in generics_with_static.params.iter_mut().enumerate() {
        if let syn::GenericParam::Lifetime(LifetimeParam {
            lifetime, bounds, ..
        }) = i
        {
            if &brand == lifetime && bounds.is_empty() {
                *lifetime = Lifetime::new("'static", Span::call_site());
                let mut new = Punctuated::new();
                for (id, param) in generics_with_no_lt.params.into_iter().enumerate() {
                    if id != idx {
                        new.push(param);
                    }
                }
                generics_with_brand_as_downcast.params[idx] =
                    GenericParam::Lifetime(LifetimeParam {
                        attrs: vec![],
                        lifetime: Lifetime::new("'__downcast", Span::call_site()),
                        colon_token: None,
                        bounds: Punctuated::new(),
                    });
                generics_with_no_lt.params = new;
            }
        }
    }

    let struct_name = format_ident!("Branded{}", item.ident.clone());
    // if item.vis == Visibility::Inherited {
    //     item.vis = parse_quote!(pub( super ));
    // }
    let orig_struct_vis = old_item.vis.clone();
    let mut raised_struct_vis = orig_struct_vis.clone();
    raise_visibility(&mut raised_struct_vis);
    item.vis = raised_struct_vis.clone();

    let all_fields = old_item
        .fields
        .iter()
        .enumerate()
        .map(|(idx, field)| -> FnArg {
            FnArg::Typed(PatType {
                attrs: Vec::new(),
                pat: Box::new(syn::Pat::Ident(PatIdent {
                    attrs: vec![],
                    by_ref: None,
                    mutability: None,
                    ident: field
                        .ident
                        .clone()
                        .unwrap_or(Ident::new(&format!("_{}", idx), Span::call_site())),
                    subpat: None,
                })),
                colon_token: Colon(Span::call_site()),
                ty: Box::new(field.ty.clone()),
            })
        })
        .collect::<Punctuated<_, Comma>>();
    let sig = Signature {
        constness: Some(Const(Span::call_site())),
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: Fn(Span::call_site()),
        ident: Ident::new("new", Span::call_site()),
        generics: Generics {
            lt_token: None,
            params: Punctuated::new(),
            gt_token: None,
            where_clause: None,
        },
        paren_token: Paren(Span::call_site()),
        inputs: all_fields,
        variadic: None,
        output: syn::ReturnType::Type(
            Token![->](Span::call_site()),
            Box::new(syn::Type::Verbatim(
                SelfType(Span::call_site()).into_token_stream(),
            )),
        ),
    };

    let construct = ExprStruct {
        attrs: Vec::new(),
        qself: None,
        path: parse_quote!(Self),
        brace_token: Brace(Span::call_site()),
        fields: {
            let mut p = Punctuated::new();
            for (idx, i) in old_item.fields.iter().enumerate() {
                p.push(FieldValue {
                    attrs: Vec::new(),
                    member: if let Some(name) = i.ident.clone() {
                        Member::Named(name)
                    } else {
                        Member::Unnamed(Index {
                            index: idx as u32,
                            span: Span::call_site(),
                        })
                    },
                    colon_token: i
                        .ident
                        .clone()
                        .map(|_| None)
                        .unwrap_or(Some(Colon(Span::call_site()))),
                    expr: {
                        let gen = Ident::new(&format!("_{}", idx), Span::call_site());
                        if let Some(name) = i.ident.clone() {
                            parse_quote!(#name)
                        } else {
                            parse_quote!(#gen)
                        }
                    },
                });
            }
            if matches!(&item.fields, Fields::Named(_)) {
                let brand_ident = format_ident!("__{}", brand.ident.clone());
                p.push(parse_quote!(#brand_ident: ::std::marker::PhantomData));
            } else {
                p.push(FieldValue {
                    attrs: Vec::new(),
                    member: Member::Unnamed(Index {
                        index: p.len() as u32,
                        span: Span::call_site(),
                    }),
                    colon_token: Some(Colon(Span::call_site())),
                    expr: parse_quote!(::std::marker::PhantomData),
                });
            }
            p
        },
        dot2_token: None,
        rest: None,
    };

    let type_no_lt = generics_with_no_lt.split_for_impl().1;
    let orig_struct_name = old_item.ident.clone();
    item.ident = struct_name.clone();

    let st = generics_with_static.split_for_impl().1;
    let ip = generics_with_no_lt.split_for_impl().0;
    let wh = item.generics.split_for_impl().2;

    let struct_def = quote! {
            use super::*;
            #item
            impl #ip #struct_name #st #wh {
                #[allow(unused)]
                #raised_struct_vis #sig {
                    #construct
                }
            }
    };
    let (imp, typ, wher) = item.generics.split_for_impl();
    let (_, dtyp, dwhere) = generics_with_brand_as_downcast.split_for_impl();
    let impl_def = quote! {
        unsafe impl #imp crate::util::branded::HasBrandLifetime<#brand> for #struct_name #typ #wher {
            type Downcast<'__downcast> = #struct_name #dtyp #dwhere;
        }
    };
    (
        quote! {
            #struct_def
                #impl_def
        },
        quote! {
            #[allow(unused)]
            #orig_struct_vis type #orig_struct_name #type_no_lt  = #mod_name::#struct_name #st;
            #[allow(unused)]
            #orig_struct_vis use #mod_name::#struct_name;
        },
    )
}
fn raise_visibility(v: &mut Visibility) {
    match v {
        Visibility::Restricted(vis_restricted) => {
            if vis_restricted.path.segments.len() == 1
                && vis_restricted.path.segments.first() == Some(&parse_quote!(crate))
            {
                return;
            }
            if vis_restricted.path.segments.len() == 1
                && vis_restricted.path.segments.first() == Some(&parse_quote!(self))
            {
                vis_restricted.path.segments.clear();
                vis_restricted.path.segments.push(parse_quote!(super));
                return;
            }
            vis_restricted.path.segments.insert(0, parse_quote!(super));
            vis_restricted.in_token = parse_quote!(in);
        }
        Visibility::Inherited => *v = parse_quote!(pub(super)),
        _ => {}
    }
}


use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_quote,
    punctuated::Punctuated,
    token::{Brace, Colon, Comma, Const, Fn, Paren, SelfType},
    ExprStruct, Field, FieldValue, Fields, FieldsUnnamed, FnArg, Generics, Ident, Index, ItemStruct, Lifetime, LifetimeParam, Member, PatIdent, PatType,
    Signature, Token, Visibility,
};
// yes the code quality is awful
// yes it works (most of the time)
pub(crate) fn derive_has_brand_lifetime_impl(name: Lifetime, mut item: ItemStruct) -> TokenStream {
    // item.fields.iter_mut().for_each(|field| {
    //     let ty = field.ty.clone();
    //     field.ty = parse_quote!(super::#ty)
    // });
    let old_item = item.clone();
    item.generics
        .params
        .push(syn::GenericParam::Lifetime(LifetimeParam::new(
            name.clone(),
        )));
    let mut f = Field {
        attrs: Vec::new(),
        vis: syn::Visibility::Inherited,
        mutability: syn::FieldMutability::None,
        ident: None,
        colon_token: None,
        ty: syn::Type::Verbatim(parse_quote!(crate::util::branded::PhantomInvariant<#name>)),
    };
    item.fields = match item.fields {
        syn::Fields::Named(mut fields_named) => {
            f.ident = Some(format_ident!("__{}", name.ident.clone()));
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
    for (idx, i) in generics_with_static.params.iter_mut().enumerate() {
        if let syn::GenericParam::Lifetime(LifetimeParam {
            lifetime, bounds, ..
        }) = i
        {
            if &name == lifetime && bounds.is_empty() {
                *lifetime = Lifetime::new("'static", Span::call_site());
                let mut new = Punctuated::new();
                for (id, param) in generics_with_no_lt.params.into_iter().enumerate() {
                    if id != idx {
                        new.push(param);
                    }
                }
                generics_with_no_lt.params = new;
            }
        }
    }
    let st = generics_with_static.split_for_impl().1;
    let ip = generics_with_no_lt.split_for_impl().0;
    let wh = item.generics.split_for_impl().2;
    let struct_name = item.ident.clone();
    let mod_name = Ident::new(
        &format!("__define_{}", item.ident.to_string().to_ascii_lowercase()),
        struct_name.span(),
    );
    if item.vis == Visibility::Inherited {
        item.vis = parse_quote!(pub( super ));
    }

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
                let brand_ident = format_ident!("__{}", name.ident.clone());
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
    let vis = item.vis.clone();
    let type_no_lt = generics_with_no_lt.split_for_impl().1;
    quote! {
        mod #mod_name {
            use super::*;
            #item
            impl #ip #struct_name #st #wh {
                pub #sig {
                    #construct
                }
            }
        }
        #vis type #struct_name #type_no_lt  = #mod_name::#struct_name #st;

    }
}

extern crate proc_macro;

use proc_macro2::Ident;
use quote::TokenStreamExt;
use syn::spanned::Spanned;
use syn::{Data, DataEnum, DeriveInput, Type};

fn gen_impl_spanned(derive_input: &DeriveInput, enum_data: &DataEnum) -> proc_macro2::TokenStream {
    let name = &derive_input.ident;
    if enum_data.variants.is_empty() {
        return proc_macro2::TokenStream::new();
    }
    let mut match_body = proc_macro2::TokenStream::new();
    for variant in &enum_data.variants {
        if variant.fields.is_empty() {
            panic!(
                "{}",
                syn::Error::new(variant.span(), "expecting at least one field")
            );
        }
        let (idx, span_field) = if let Some((idx, span_field)) =
            variant.fields.iter().enumerate().find(|(_, field)| {
                field
                    .attrs
                    .iter()
                    .filter_map(|attr| attr.meta.require_path_only().ok())
                    .filter_map(|meta| {
                        if meta.segments.len() == 1 {
                            meta.segments.last()
                        } else {
                            None
                        }
                    })
                    .any(|segment| segment.ident == "span")
            }) {
            (idx, span_field)
        } else {
            variant
                .fields
                .iter()
                .enumerate()
                .next()
                .expect("expecting at least one field")
        };
        let variant_name = &variant.ident;
        let mut fields_body = proc_macro2::TokenStream::new();
        for (idx2, field) in variant.fields.iter().enumerate() {
            let ident = Ident::new(&format!("_field_{}", idx2), field.span());
            if idx2 == 0 {
                fields_body.append_all(quote::quote! {#ident});
            } else {
                fields_body.append_all(quote::quote! {, #ident});
            }
        }
        let span_field_name = Ident::new(&format!("_field_{}", idx), span_field.span());
        let mut body = proc_macro2::TokenStream::new();
        if let Type::Path(path) = &span_field.ty {
            if path.path.segments.last().unwrap().ident == "Span" {
                body.append_all(quote::quote! {#span_field_name.clone()});
            } else {
                body.append_all(quote::quote! {#span_field_name.into()});
            }
        } else {
            body.append_all(quote::quote! {#span_field_name.into()});
        }
        match_body.append_all(quote::quote! {
            #name::#variant_name(#fields_body) => {#body}
        });
    }
    quote::quote! {
        #[automatically_derived]
        impl crate::private::Spanned for #name {
            fn span(&self) -> ::proc_macro2::Span {
                #[allow(unused_variables, deprecated, clippy::used_underscore_binding)]
                match self {
                    #match_body
                }
            }
        }
    }
}

fn impl_declare_parser_error(derive_input: DeriveInput) -> proc_macro2::TokenStream {
    let Data::Enum(enum_data) = &derive_input.data else {
        panic!("CrispParserError macro only supports enums");
    };
    let name = &derive_input.ident;
    let gen_spanned = gen_impl_spanned(&derive_input.clone(), enum_data);
    quote::quote! {
        #gen_spanned

        #[automatically_derived]
        impl From<#name> for syn::Error {
            fn from(value: #name) -> Self {
                crate::private::SynError::from(value).into()
            }
        }

        #[automatically_derived]
        impl From<#name> for ::proc_macro2::Span {
            fn from(value: #name) -> Self {
                value.span()
            }
        }

        #[automatically_derived]
        impl From<&#name> for ::proc_macro2::Span {
            fn from(value: &#name) -> Self {
                value.span()
            }
        }
    }
}

#[proc_macro_derive(CrispParserError, attributes(span))]
pub fn declare_parser_error(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    impl_declare_parser_error(syn::parse_macro_input!(input)).into()
}

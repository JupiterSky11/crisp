use std::fmt::Debug;

use quote::quote;
use syn::{
    parse::{
        Parse,
        ParseStream,
    },
    parenthesized,
    LitInt,
};
use proc_macro2::TokenStream as TokenStream2;

pub mod interp;
pub mod parser;
pub use num;

#[cfg_attr(feature = "debug", derive(Debug))]
pub struct Crisp {
    contents: Vec<CrispType>,
}

impl Parse for Crisp {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        Ok(Crisp {
            contents: parse_vec(input),
        })
    }
}

impl Crisp {
    pub fn to_rust(self) -> TokenStream2 {
        let mut stream = TokenStream2::new();
        let list = self.contents;
        let output = quote! { #(#list) * };
        stream.extend(output);
        stream
    }
}

use quote::ToTokens;
impl ToTokens for Crisp {
    fn to_tokens(&self, input: &mut TokenStream2) {
        let output = quote!{self.contents};
        input.extend(output)
    }
}

fn parse_vec<T: Parse>(input: ParseStream) -> Vec<T> {
    let mut output: Vec<T> = Vec::new();
    println!("Adding stream as elements of vector.");
    while let Ok(x) = input.parse() {
        output.push(x);
    }
    println!("Returning vector.");
    output
}

#[derive(Clone)]
pub struct CrispFn {
    pub name: String,
    pub args: Vec<(String, CrispType)>,
    pub body: Vec<CrispType>,
}

impl Debug for CrispFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut args = String::new();
        for (i, arg) in self.args.iter().enumerate() {
            if i == 0 {
                args += format!("{:?}", &arg).as_str();
            } else {
                args += format!(", {:?}", &arg).as_str();
            }
        }
        f.debug_tuple("CrispFn").field(&format!("fn ({}) {{{:?}}}", args, self.body)).finish()
    }
}

impl Parse for CrispFn {
    fn parse(_input: ParseStream) -> syn::Result<Self> {
        todo!()
    }
}

impl ToTokens for CrispFn {
    fn to_tokens(&self, input: &mut TokenStream2) {
        let output = quote! {{}};
        input.extend(output);
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub enum CrispType {
    Fn(CrispFn),
    Int(i32),
    List(Option<Vec<CrispType>>),
}

impl Parse for CrispType {
    fn parse(input: syn::parse::ParseStream) -> Result<Self, syn::Error> {
        // if let Ok(_) = input.parse::<Token![+]>() {
        //     let args = parse_vec(input);
        //     return Ok(CrispType::Fn(CrispFn { name: "+".into(), args, body: todo!() }))
        // }
        
        if let Ok(l) = parse_for_paren(input) {
            return Ok(CrispType::List(Some(l)))
        };

        let t = input.parse::<LitInt>()?;
        Ok(CrispType::Int(t.base10_parse::<i32>()?))
    }
}

fn parse_for_paren(input: ParseStream) -> Result<Vec<CrispType>, syn::Error> {
    let inner;
    parenthesized!(inner in input);
    Ok(parse_vec(&inner))
}

impl ToTokens for CrispType {
    fn to_tokens(&self, input: &mut TokenStream2) {
        let output = match self {
            CrispType::Fn(fn_)      => quote!{CrispType::Fn(#fn_)},
            CrispType::Int(x)  => quote!{CrispType::Int(#x)},
            CrispType::List(Some(x)) => quote!{ (#(#x) *) },
            _ => TokenStream2::new(),
        };
        input.extend(output)
    }
}

// #[cfg(feature = "debug")]
// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_parse_str() {
//         let t: CrispType = syn::parse_str("(+ 1 2)").unwrap();
//         assert_eq!(CrispType::List(vec![CrispType::Fn, CrispType::Int(1), CrispType::Int(2)]), t);
//     }

//     #[test]
//     fn test_parse_macro() {
//         let t: Crisp = syn::parse_str("#[(+ 1 2) (+ 3 4)]").unwrap();
//         assert_eq !(Crisp { contents: vec![CrispType::List(vec![CrispType::Fn, CrispType::Int(1), CrispType::Int(2)]), CrispType::List(vec![CrispType::Fn, CrispType::Int(3), CrispType::Int(4)])] }, t);
//     }
// }

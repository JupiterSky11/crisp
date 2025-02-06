use quote::quote;
use syn::{
    Token,
    parse::{
        Parse,
        ParseStream,
    },
    parenthesized,
    LitInt,
};
use proc_macro2::TokenStream as TokenStream2;

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
    pub fn eval(self) -> Crisp {
        Crisp { contents: vec![CrispType::List(self.contents).eval()] }
    }
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

pub enum CrispType {
    Fn,
    Int(i32),
    List(Vec<CrispType>),
}

impl CrispType {
    fn eval(&self) -> CrispType {
        match self {
            CrispType::List(l) => {
                println!("Length {}", l.len());
                if l.len() >= 3 {
                    match l[0] {
                        CrispType::Fn => {
                            let a = match l[1].eval() {
                                CrispType::Int(x) => x,
                                _ => panic!(),
                            };
                            let b = match l[2].eval() {
                                CrispType::Int(x) => x,
                                _ => panic!(),
                            };
                            // --- Preferred solution on Rust version 2024
                            // if let CrispType::Int(a) = l[1] && let CrispType::Int(b) = l[2] {
                            //     Crisp { contents: List { contents: vec![CrispType::Int(a + b)] } }
                            // } else { panic!() }
                            CrispType::Int(a + b)
                        },
                        _ => panic!(),
                    }
                } else { panic!() }
            },
            CrispType::Int(x) => CrispType::Int(*x),
            _ => CrispType::Fn,
        }
    }
}

impl Parse for CrispType {
    fn parse(input: syn::parse::ParseStream) -> Result<Self, syn::Error> {
        if let Ok(_) = input.parse::<Token![+]>() {
            return Ok(CrispType::Fn)
        }
        
        if let Ok(l) = parse_for_paren(input) {
            return Ok(CrispType::List(l))
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
            CrispType::Fn      => quote!{CrispType::Fn},
            CrispType::Int(x)  => quote!{CrispType::Int(#x)},
            CrispType::List(x) => quote!{ (#(#x) *) },
            _ => TokenStream2::new(),
        };
        input.extend(output)
    }
}

use proc_macro2::TokenStream;
use quote::{ToTokens, TokenStreamExt};
use syn::parenthesized;
use syn::parse::{Parse, ParseStream, Parser};
use syn::token::Paren;
use crate::parser::CrispToken;

#[derive(Debug, Clone)]
pub enum CrispExpr {
    List(CrispExprList),
    Token(CrispToken),
    String(syn::LitStr),
}

impl Parse for CrispExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(list) = CrispExprList::parse(input) {
            Ok(CrispExpr::List(list))
        } else if let Ok(token) = CrispToken::parse(input) {
            Ok(CrispExpr::Token(token))
        } else if let Ok(string) = input.parse::<syn::LitStr>() {
            Ok(CrispExpr::String(string))
        } else {
            Err(input.error("Expected Crisp expression"))
        }
    }
}

impl ToTokens for CrispExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            CrispExpr::List(list) => list.to_tokens(tokens),
            CrispExpr::Token(token) => token.to_tokens(tokens),
            CrispExpr::String(string) => string.to_tokens(tokens),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CrispExprList {
    pub paren: Paren,
    pub content: Vec<CrispExpr>,
}

impl Parse for CrispExprList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let inner;
        parenthesized!(inner in input);
        let mut content = vec![];
        while let Ok(expr) = CrispExpr::parse(&inner) {
            content.push(expr);
        }
        Ok(CrispExprList { paren: Default::default(), content })
    }
}

impl ToTokens for CrispExprList {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if self.content.is_empty() {
            tokens.append_all(quote::quote! {::crisp::interp::CrispNil::new()});
        } else {
            let mut params_body = TokenStream::new();
            for (i, param) in self.content.iter().skip(1).enumerate() {
                if i == 0 {
                    params_body.extend(quote::quote! {#param});
                } else {
                    params_body.extend(quote::quote! {, #param});
                }
            }
            let func = self.content.first().unwrap();
            // TODO Look through the existing symbols an call the right one, otherwise just use the symbol itself 
            tokens.append_all(quote::quote! {#func(#params_body)});
        }
    }
}

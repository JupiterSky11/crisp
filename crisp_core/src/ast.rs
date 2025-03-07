use std::io::Cursor;
use proc_macro2::{Delimiter, TokenStream};
use syn::parenthesized;
use syn::parse::{Parse, ParseStream, Parser};
use syn::parse::discouraged::Speculative;
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

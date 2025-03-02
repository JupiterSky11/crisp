use syn::token::Paren;
use crate::parser::CrispToken;

#[derive(Debug, Clone)]
pub enum CrispExpr {
    List(CrispExprList),
    Token(CrispToken),
}

#[derive(Debug, Clone)]
pub struct CrispExprList {
    pub paren: Paren,
    pub content: Vec<CrispExpr>,
}

#[derive(Debug, Clone)]
pub struct CrispFn { }

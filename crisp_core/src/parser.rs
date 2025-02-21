use std::ops::Deref;
use proc_macro2::{Delimiter, Span, TokenTree};
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{Error, LitFloat, LitInt};

fn parse_integer_from_token_tree(tt: TokenTree, span: Span) -> syn::Result<LitInt> {
    if let TokenTree::Literal(lit) = tt {
        let lit2 = lit.clone();
        litrs::IntegerLit::try_from(lit).or(Err(syn::Error::new(span, "Expected integer literal")))?;
        Ok(LitInt::from(lit2))
    } else {
        Err(syn::Error::new(span, "Expected integer literal"))
    }
}

fn parse_float_from_token_tree(tt: TokenTree, span: Span) -> syn::Result<LitFloat> {
    if let TokenTree::Literal(lit) = tt {
        let lit2 = lit.clone();
        litrs::FloatLit::try_from(lit).or(Err(syn::Error::new(span, "Expected float literal")))?;
        Ok(LitFloat::from(lit2))
    } else {
        Err(syn::Error::new(span, "Expected float literal"))
    }
}

fn parse_sign<'a>((tt, next): (TokenTree, syn::buffer::Cursor<'a>), _span: Span) -> syn::Result<(Option<bool>, TokenTree, syn::buffer::Cursor<'a>)> {
    if let TokenTree::Punct(punct) = tt {
        let Some((tt, next)) = next.token_tree() else {
            return Err(syn::Error::new(punct.span(), "Expected number after punctuation"));
        };
        match punct.as_char() {
            '+' => Ok((Some(false), tt, next)),
            '-' => Ok((Some(true), tt, next)),
            _ => Err(syn::Error::new(punct.span(), "Expected '+' or '-' before number"))
        }
    } else {
        Ok((None, tt, next))
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub enum Rational {
    Integer{negative: Option<bool>, number: LitInt},
    Ratio{negative: Option<bool>, numerator: LitInt, denominator: LitInt},
}

fn check_rational<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(input: C) -> syn::Result<(Rational, syn::buffer::Cursor<'a>)> {
    if let Some((tt, next)) = input.token_tree() {
        let (negative, tt, next) = parse_sign((tt, next), next.span())?;
        let numerator = parse_integer_from_token_tree(tt, next.span())?;
        if let Some((punct, next2)) = next.punct() {
            if punct.as_char() == '/' {
                if let Some((tt2, next3)) = next2.token_tree() {
                    let denominator = parse_integer_from_token_tree(tt2, next3.span())?;
                    return Ok((Rational::Ratio { negative, numerator, denominator }, next3));
                } else {
                    return Err(syn::Error::new(next2.span(), "Expected integer literal"));
                }
            }
        }
        Ok((Rational::Integer { negative, number: numerator }, next))
    } else {
        Err(syn::Error::new(input.span(), "Expected Rational"))
    }
}

impl Parse for Rational {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_rational(cursor))
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub enum Real {
    Float{negative: Option<bool>, number: LitFloat},
    Rational(Rational),
}

fn check_real<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(input: C) -> syn::Result<(Real, syn::buffer::Cursor<'a>)> {
    let input = *input;
    if let Ok((rational, next)) = check_rational(&input) {
        Ok((Real::Rational(rational), next))
    } else if let Some((tt, next)) = input.token_tree() {
        let (negative, tt, next) = parse_sign((tt, next), next.span())?;
        let float = parse_float_from_token_tree(tt, next.span())?;
        return Ok((Real::Float {negative, number:float}, next));
    } else {
        return Err(syn::Error::new(input.span(), "Expected real literal"));
    }
}

impl Parse for Real {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_real(cursor))
    }
}

impl From<Rational> for Real {
    fn from(value: Rational) -> Self {
        Real::Rational(value)
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub enum Complex {
    Float { real_negative: Option<bool>, real: LitFloat, imaginary_negative: Option<bool>, imaginary: LitFloat },
    Rational { real: Rational, imaginary: Rational },
}

fn check_complex<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(input: C) -> syn::Result<(Complex, syn::buffer::Cursor<'a>)> {
    if let Some((punct, next)) = input.punct() {
        if punct.as_char() != '#' {
            return Err(syn::Error::new(input.span(), "Expected '#C'"));
        };
        let next = if let Some((_, next2)) = next.ident().filter(|(ident, _)| ident == "C") {
            next2
        } else {
            return Err(syn::Error::new(input.span(), "Expected '#C'"));
        };
        if let Some((inner, _, next2)) = next.group(Delimiter::Parenthesis) {
            if let Ok((real, inner2)) = check_real(&inner) {
                match check_real(&inner2) {
                    Err(err) => Err(err),
                    Ok((Real::Float {negative: imaginary_negative, number: imaginary}, next3)) => {
                        if !next3.eof() {
                            return Err(syn::Error::new(next3.span(), "Expected ')'"));
                        }
                        match real {
                            Real::Float { negative: real_negative, number: real } => {
                                Ok((Complex::Float {real_negative, real, imaginary_negative, imaginary}, next2))
                            }
                            Real::Rational(_) => Err(syn::Error::new(next3.span(), "Expected float")),
                        }
                    }
                    Ok((Real::Rational(imaginary), next3)) => {
                        if !next3.eof() {
                            return Err(syn::Error::new(next3.span(), "Expected ')'"));
                        }
                        match real {
                            Real::Float { .. } => Err(syn::Error::new(next3.span(), "Expected rational")),
                            Real::Rational(real) => {
                                Ok((Complex::Rational {real, imaginary}, next2))
                            }
                        }
                    }
                }
            } else {
                Err(syn::Error::new(input.span(), "Expected real"))
            }
        } else {
            Err(syn::Error::new(input.span(), "Expected '('"))
        }
    } else {
        Err(syn::Error::new(input.span(), "Expected complex literal"))
    }
}

impl Parse for Complex {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_complex(cursor))
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub enum Number {
    Real(Real),
    Complex(Complex),
}

fn check_number<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(input: C) -> syn::Result<(Number, syn::buffer::Cursor<'a>)> {
    let input = *input;
    if let Ok((complex, next)) = check_complex(&input) {
        Ok((Number::Complex(complex), next))
    } else if let Ok((real, next)) = check_real(&input) {
        Ok((Number::Real(real), next))
    } else {
        Err(syn::Error::new(input.span(), "Expected number literal"))
    }
}

impl Parse for Number {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_number(cursor))
    }
}

impl From<Real> for Number {
    fn from(value: Real) -> Self {
        Number::Real(value)
    }
}

impl TryFrom<Number> for Real {
    type Error = ();

    fn try_from(value: Number) -> Result<Self, Self::Error> {
        match value {
            Number::Real(real) => Ok(real),
            _ => Err(()),
        }
    }
}

impl From<Rational> for Number {
    fn from(value: Rational) -> Self {
        Number::Real(Real::from(value))
    }
}

// Symbol

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub struct Symbol {
    ident: syn::Ident,
}

fn check_symbol<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(input: C) -> syn::Result<(Symbol, syn::buffer::Cursor<'a>)> {
    if let Some((ident, next)) = input.ident() {
        Ok((Symbol { ident }, next))
    } else {
        Err(Error::new(input.span(), "expected identifier"))
    }
}

impl Parse for Symbol {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_symbol(cursor))
    }
}

// Token

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub enum CrispToken {
    Number(Number),
    Symbol(Symbol),
}

fn check_token<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(input: C) -> syn::Result<(CrispToken, syn::buffer::Cursor<'a>)> {
    let input = *input;
    if let Ok((number, next)) = check_number(&input) {
        Ok((CrispToken::Number(number), next))
    } else if let Ok((symbol, next)) = check_symbol(&input) {
        Ok((CrispToken::Symbol(symbol), next))
    } else {
        Err(Error::new(input.span(), "Expected token"))
    }
}

impl Parse for CrispToken {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_token(cursor))
    }
}

impl ToTokens for CrispToken {
    fn to_tokens(&self, _tokens: &mut proc_macro2::TokenStream) {
        match self {
            CrispToken::Number(n) => {
                let n: Number = n.clone();
                let _tokens_number: proc_macro2::TokenStream = match n {
                    Number::Real(r) => {
                        let r: Real = r.clone();
                        match r {
                            Real::Float{..} => todo!(),
                            Real::Rational(_) => todo!(),
                        }
                    }
                    Number::Complex(c) => {
                        let c = c.clone();
                        match c {
                            Complex::Float { .. } => todo!(),
                            Complex::Rational { ..  } => todo!(),
                        }
                    },
                };
                //tokens_number.to_tokens(tokens);
            }
            CrispToken::Symbol(_) => {}
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::{CrispToken, Number};

    #[test]
    fn can_parse_integer() {
        syn::parse_str::<CrispToken>("-1.23").expect("Not a number");
        syn::parse_str::<CrispToken>("3.14f64").expect("Not a number");
    }

    #[test]
    fn can_parse_rational() {
        syn::parse_str::<CrispToken>("4/56").expect("Not a number");
    }

    #[test]
    fn can_parse_real() {
        syn::parse_str::<CrispToken>("3.14f64").expect("Not a number");
        syn::parse_str::<CrispToken>("42u128").expect("Not a number");
        syn::parse_str::<CrispToken>("-1/12u8").expect("Not a number");
        syn::parse_str::<CrispToken>("4e1025").expect("Not a number");
    }

    #[test]
    fn can_parse_complex() {
        syn::parse_str::<CrispToken>("#C(3.14 -1.0)").expect("Not a number");
    }

    #[test]
    fn can_parse_big_integers() {
        syn::parse_str::<CrispToken>("100000000000000000000000000000000000000000000000000000000000000000000000000000000").expect("Not a number");
    }

    #[test]
    fn can_parse_big_bigfloats() {
        syn::parse_str::<CrispToken>("4.33333333333333333333333333333333333333333333333333333333333333333333333e100000000000000000000000000000000000000000000000000000000").expect("Not a number");
    }

    #[test]
    fn errors_on_wrong_rational_format() {
        // A rational has to be two integers
        let num = syn::parse_str::<CrispToken>("1/1.2");
        assert!(num.is_err());
        let num = syn::parse_str::<CrispToken>("2.3/1");
        assert!(num.is_err());
        let num = syn::parse_str::<CrispToken>("4.5/6.7");
        assert!(num.is_err());
    }
}
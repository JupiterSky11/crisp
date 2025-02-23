use crate::parser::_inner::*;
use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::{LitFloat, LitInt};

pub mod _inner {
    use crate::parser::{Complex, CrispToken, Number, Rational, Real, Symbol};
    use proc_macro2::{Delimiter, Span, TokenTree};
    use std::ops::Deref;
    use syn::{Error, LitFloat, LitInt};

    pub fn parse_integer_from_token_tree(tt: TokenTree, span: Span) -> syn::Result<LitInt> {
        if let TokenTree::Literal(lit) = tt {
            let lit2 = lit.clone();
            litrs::IntegerLit::try_from(lit)
                .or(Err(syn::Error::new(span, "Expected integer literal")))?;
            Ok(LitInt::from(lit2))
        } else {
            Err(syn::Error::new(span, "Expected integer literal"))
        }
    }

    pub fn parse_float_from_token_tree(tt: TokenTree, span: Span) -> syn::Result<LitFloat> {
        if let TokenTree::Literal(lit) = tt {
            let lit2 = lit.clone();
            litrs::FloatLit::try_from(lit)
                .or(Err(syn::Error::new(span, "Expected float literal")))?;
            Ok(LitFloat::from(lit2))
        } else {
            Err(syn::Error::new(span, "Expected float literal"))
        }
    }

    pub fn parse_sign(
        (tt, next): (TokenTree, syn::buffer::Cursor),
        _span: Span,
    ) -> syn::Result<(Option<bool>, TokenTree, syn::buffer::Cursor)> {
        if let TokenTree::Punct(punct) = tt {
            let Some((tt, next)) = next.token_tree() else {
                return Err(syn::Error::new(
                    punct.span(),
                    "Expected number after punctuation",
                ));
            };
            match punct.as_char() {
                '+' => Ok((Some(false), tt, next)),
                '-' => Ok((Some(true), tt, next)),
                _ => Err(syn::Error::new(
                    punct.span(),
                    "Expected '+' or '-' before number",
                )),
            }
        } else {
            Ok((None, tt, next))
        }
    }

    pub fn check_rational<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> syn::Result<(Rational, syn::buffer::Cursor<'a>)> {
        if let Some((tt, next)) = input.token_tree() {
            let (negative, tt, next) = parse_sign((tt, next), next.span())?;
            let numerator = parse_integer_from_token_tree(tt, next.span())?;
            if let Some((punct, next2)) = next.punct() {
                if punct.as_char() == '/' {
                    if let Some((tt2, next3)) = next2.token_tree() {
                        let denominator = parse_integer_from_token_tree(tt2, next3.span())?;
                        return Ok((
                            Rational::Ratio {
                                negative,
                                numerator,
                                denominator,
                            },
                            next3,
                        ));
                    } else {
                        return Err(syn::Error::new(next2.span(), "Expected integer literal"));
                    }
                }
            }
            Ok((
                Rational::Integer {
                    negative,
                    number: numerator,
                },
                next,
            ))
        } else {
            Err(syn::Error::new(input.span(), "Expected Rational"))
        }
    }

    pub fn check_real<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> syn::Result<(Real, syn::buffer::Cursor<'a>)> {
        let input = *input;
        if let Ok((rational, next)) = check_rational(&input) {
            Ok((Real::Rational(rational), next))
        } else if let Some((tt, next)) = input.token_tree() {
            let (negative, tt, next) = parse_sign((tt, next), next.span())?;
            let float = parse_float_from_token_tree(tt, next.span())?;
            return Ok((
                Real::Float {
                    negative,
                    number: float,
                },
                next,
            ));
        } else {
            return Err(syn::Error::new(input.span(), "Expected real literal"));
        }
    }

    pub fn check_complex<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> syn::Result<(Complex, syn::buffer::Cursor<'a>)> {
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
                        Ok((
                            Real::Float {
                                negative: imaginary_negative,
                                number: imaginary,
                            },
                            next3,
                        )) => {
                            if !next3.eof() {
                                return Err(syn::Error::new(next3.span(), "Expected ')'"));
                            }
                            match real {
                                Real::Float {
                                    negative: real_negative,
                                    number: real,
                                } => Ok((
                                    Complex::Float {
                                        real_negative,
                                        real,
                                        imaginary_negative,
                                        imaginary,
                                    },
                                    next2,
                                )),
                                Real::Rational(_) => {
                                    Err(syn::Error::new(next3.span(), "Expected float"))
                                }
                            }
                        }
                        Ok((Real::Rational(imaginary), next3)) => {
                            if !next3.eof() {
                                return Err(syn::Error::new(next3.span(), "Expected ')'"));
                            }
                            match real {
                                Real::Float { .. } => {
                                    Err(syn::Error::new(next3.span(), "Expected rational"))
                                }
                                Real::Rational(real) => {
                                    Ok((Complex::Rational { real, imaginary }, next2))
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

    pub fn check_number<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> syn::Result<(Number, syn::buffer::Cursor<'a>)> {
        let input = *input;
        if let Ok((complex, next)) = check_complex(&input) {
            Ok((Number::Complex(complex), next))
        } else if let Ok((real, next)) = check_real(&input) {
            Ok((Number::Real(real), next))
        } else {
            Err(syn::Error::new(input.span(), "Expected number literal"))
        }
    }

    pub fn check_symbol<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> syn::Result<(Symbol, syn::buffer::Cursor<'a>)> {
        if let Some((ident, next)) = input.ident() {
            Ok((Symbol { ident }, next))
        } else {
            Err(Error::new(input.span(), "expected identifier"))
        }
    }

    pub fn check_token<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> syn::Result<(CrispToken, syn::buffer::Cursor<'a>)> {
        let input = *input;
        if let Ok((number, next)) = check_number(&input) {
            Ok((CrispToken::Number(number), next))
        } else if let Ok((symbol, next)) = check_symbol(&input) {
            Ok((CrispToken::Symbol(symbol), next))
        } else {
            Err(Error::new(input.span(), "Expected token"))
        }
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub enum Rational {
    Integer {
        negative: Option<bool>,
        number: LitInt,
    },
    Ratio {
        negative: Option<bool>,
        numerator: LitInt,
        denominator: LitInt,
    },
}

impl Rational {
    fn is_negative(&self) -> bool {
        matches!(
            self,
            Rational::Integer {
                negative: Some(true),
                ..
            } | Rational::Ratio {
                negative: Some(true),
                ..
            }
        )
    }
}

impl Parse for Rational {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_rational(cursor))
    }
}

impl ToTokens for Rational {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let is_negative = self.is_negative();
        match self {
            Rational::Integer { number, .. } => {
                let formatted = number.to_string();
                if is_negative {
                    tokens.append_all(quote! {::core::ops::Neg::neg(<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted).unwrap())})
                } else {
                    tokens.append_all(quote! {<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted).unwrap()})
                }
            }
            Rational::Ratio {
                numerator,
                denominator,
                ..
            } => {
                let formatted_numerator = numerator.to_string();
                let formatted_denominator = denominator.to_string();
                if is_negative {
                    tokens.append_all(quote! {::crisp_core::num::BigRational::new(::core::ops::Neg::neg(<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_numerator).unwrap()),<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_denominator).unwrap())});
                } else {
                    tokens.append_all(quote! {::crisp_core::num::BigRational::new(<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_numerator).unwrap(),<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_denominator).unwrap())});
                }
            }
        };
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub enum Real {
    Float {
        negative: Option<bool>,
        number: LitFloat,
    },
    Rational(Rational),
}

impl Real {
    fn is_negative(&self) -> bool {
        match self {
            Real::Rational(rational) => rational.is_negative(),
            Real::Float {
                negative: Some(true),
                ..
            } => true,
            _ => false,
        }
    }
}

impl Parse for Real {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_real(cursor))
    }
}

impl ToTokens for Real {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let is_negative = self.is_negative();
        match self {
            Real::Float { number, .. } => {
                let formatted = format!("{}{}", if is_negative { "-" } else { "" }, number);
                tokens.append_all(quote! {<::crisp_core::num::BigFloat as ::core::str::FromStr>::from_str(#formatted).unwrap()});
            }
            Real::Rational(rational) => tokens.append_all(quote! {#rational}),
        };
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
    Float {
        real_negative: Option<bool>,
        real: LitFloat,
        imaginary_negative: Option<bool>,
        imaginary: LitFloat,
    },
    Rational {
        real: Rational,
        imaginary: Rational,
    },
}

impl Parse for Complex {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_complex(cursor))
    }
}

impl ToTokens for Complex {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Complex::Float {
                real_negative,
                real,
                imaginary_negative,
                imaginary,
            } => {
                let formatted_real = real.to_string();
                let formatted_imaginary = imaginary.to_string();
                match real_negative {
                    Some(true) => {
                        match imaginary_negative {
                            Some(true) => {
                                tokens.append_all(quote! {::crisp_core::num::Complex::new(::core::ops::Neg::neg(<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_real).unwrap()),::core::ops::Neg::neg(<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_imaginary).unwrap()))})
                            },
                            _ => {
                                tokens.append_all(quote! {::crisp_core::num::Complex::new(::core::ops::Neg::neg(<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_real).unwrap()), <::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_imaginary).unwrap())})
                            }
                        }
                    },
                    _ => {
                        match imaginary_negative {
                            Some(true) => {
                                tokens.append_all(quote! {::crisp_core::num::Complex::new(<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_real).unwrap(),::core::ops::Neg::neg(<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_imaginary).unwrap()))})
                            },
                            _ => {
                                tokens.append_all(quote! {::crisp_core::num::Complex::new(<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_real).unwrap(),<::crisp_core::num::BigInt as ::core::str::FromStr>::from_str(#formatted_imaginary).unwrap())})
                            }
                        }
                    }
                }
            }
            Complex::Rational { real, imaginary } => {
                tokens.append_all(quote! {::crisp_core::num::Complex::new(#real, #imaginary)})
            }
        }
    }
}

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub enum Number {
    Real(Real),
    Complex(Complex),
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

impl ToTokens for Number {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Number::Real(real) => tokens.append_all(quote! { #real }),
            Number::Complex(complex) => tokens.append_all(quote! { #complex }),
        }
    }
}

// Symbol

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub struct Symbol {
    ident: syn::Ident,
}

impl Parse for Symbol {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_symbol(cursor))
    }
}

impl ToTokens for Symbol {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.ident;
        tokens.append(ident.clone());
    }
}

// Token

#[cfg_attr(feature = "debug", derive(Debug))]
#[derive(Clone)]
pub enum CrispToken {
    Number(Number),
    Symbol(Symbol),
}

impl Parse for CrispToken {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.step(|cursor| check_token(cursor))
    }
}

impl ToTokens for CrispToken {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            CrispToken::Number(number) => {
                number.to_tokens(tokens);
            }
            CrispToken::Symbol(symbol) => {
                symbol.to_tokens(tokens);
            }
        }
        println!("{}", tokens);
    }
}

#[cfg(test)]
mod test {
    use crate::parser::CrispToken;

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
        syn::parse_str::<CrispToken>(
            "100000000000000000000000000000000000000000000000000000000000000000000000000000000",
        )
        .expect("Not a number");
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

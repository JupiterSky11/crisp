use crate::parser::_inner::*;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::{LitFloat, LitInt};

pub(self) mod _inner {
    use crate::parser::{Complex, CrispToken, Number, Rational, Real, Symbol};
    use crate::private::Spanned;
    use core::fmt::Debug;
    use core::ops::Deref;
    use crisp_core_macro::CrispParserError;
    use proc_macro2::{Delimiter, Span, TokenTree};
    use syn::{LitFloat, LitInt};
    use thiserror::Error;

    #[derive(Debug, Error, CrispParserError)]
    pub enum ParseIntError {
        #[error("expected an integer")]
        NotInt(Span),
        #[error("unexpectedly reached end of input")]
        Empty(Span),
    }

    pub fn parse_integer_from_token_tree(
        tt: TokenTree,
        span: Span,
    ) -> Result<LitInt, ParseIntError> {
        if let TokenTree::Literal(lit) = tt {
            let lit2 = lit.clone();
            litrs::IntegerLit::try_from(lit).or(Err(ParseIntError::NotInt(span)))?;
            Ok(LitInt::from(lit2))
        } else {
            Err(ParseIntError::Empty(span))
        }
    }

    #[derive(Debug, Error, CrispParserError)]
    pub enum ParseFloatError {
        #[error("expected a float")]
        NotFloat(Span),
        #[error("unexpectedly reached end of input")]
        Empty(Span),
    }

    pub fn parse_float_from_token_tree(
        tt: TokenTree,
        span: Span,
    ) -> Result<LitFloat, ParseFloatError> {
        if let TokenTree::Literal(lit) = tt {
            let lit2 = lit.clone();
            litrs::FloatLit::try_from(lit).or(Err(ParseFloatError::NotFloat(span)))?;
            Ok(LitFloat::from(lit2))
        } else {
            Err(ParseFloatError::Empty(span))
        }
    }

    #[derive(Debug, Error, CrispParserError)]
    pub enum ParseSignError {
        // Case where there is a punctuation, but it not either of '+' or '-'.
        #[error("expected '+' or '-' before the number, got '{1}'")]
        UnknownPunct(Span, char),
        // Case where we have the right punctuation ('+' or '-'), but there is no following token.
        #[error("expected a number literal after the sign")]
        MissingNumber(Span),
    }

    pub fn parse_negative(
        (tt, next): (TokenTree, syn::buffer::Cursor),
        _span: Span,
    ) -> Result<(Option<bool>, TokenTree, syn::buffer::Cursor), ParseSignError> {
        if let TokenTree::Punct(punct) = tt {
            let negative = match punct.as_char() {
                '+' => Some(false),
                '-' => Some(true),
                p => return Err(ParseSignError::UnknownPunct(punct.span(), p)),
            };
            let Some((tt, next)) = next.token_tree() else {
                return Err(ParseSignError::MissingNumber(next.span()));
            };
            Ok((negative, tt, next))
        } else {
            Ok((None, tt, next))
        }
    }

    #[derive(Debug, Error, CrispParserError)]
    pub enum ParseRationalError {
        #[error("{0}")]
        ParseSignError(#[from] ParseSignError),
        #[error("{0}")]
        ParseIntError(#[from] ParseIntError),
        #[error("missing denominator")]
        MissingDenominator(Span),
        #[error("unexpectedly reached end of input")]
        Empty(Span),
    }

    pub fn check_rational<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> Result<(Rational, syn::buffer::Cursor<'a>), ParseRationalError> {
        if let Some((tt, next)) = input.token_tree() {
            let (negative, tt, next) = match parse_negative((tt, next), next.span()) {
                Ok(ret) => Ok(ret),
                Err(err) => Err(ParseRationalError::ParseSignError(err)),
            }?;
            let numerator = match parse_integer_from_token_tree(tt, next.span()) {
                Ok(num) => Ok(num),
                Err(err @ ParseIntError::NotInt(_)) => Err(ParseRationalError::ParseIntError(err)),
                Err(ParseIntError::Empty(span)) => Err(ParseRationalError::Empty(span)),
            }?;
            if let Some((punct, next2)) = next.punct() {
                if punct.as_char() == '/' {
                    return if let Some((tt2, next3)) = next2.token_tree() {
                        match parse_integer_from_token_tree(tt2, next3.span()) {
                            Ok(denominator) => Ok((
                                Rational::Ratio {
                                    negative,
                                    numerator,
                                    denominator,
                                },
                                next3,
                            )),
                            Err(ParseIntError::NotInt(span)) | Err(ParseIntError::Empty(span)) => {
                                Err(ParseRationalError::MissingDenominator(span))
                            }
                        }
                    } else {
                        Err(ParseRationalError::MissingDenominator(next2.span()))
                    };
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
            Err(ParseRationalError::Empty(input.span()))
        }
    }

    #[derive(Debug, Error, CrispParserError)]
    pub enum ParseRealError {
        #[error("{0}")]
        ParseSignError(#[from] ParseSignError),
        #[error("{0}")]
        ParseRationalError(#[from] ParseRationalError),
        #[error("expected real literal")]
        NotReal(Span),
        #[error("unexpectedly reached end of input")]
        Empty(Span),
    }

    pub fn check_real<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> Result<(Real, syn::buffer::Cursor<'a>), ParseRealError> {
        let input = *input;
        if let Some((tt, next)) = input.token_tree() {
            let (negative, tt, next) = match parse_negative((tt, next), next.span()) {
                Ok(ret) => Ok(ret),
                Err(err @ ParseSignError::UnknownPunct(_, _)) => {
                    Err(ParseRealError::ParseSignError(err))
                }
                Err(ParseSignError::MissingNumber(span)) => Err(ParseRealError::Empty(span)),
            }?;
            match parse_float_from_token_tree(tt, next.span()) {
                Ok(float) => {
                    return Ok((
                        Real::Float {
                            negative,
                            number: float,
                        },
                        next,
                    ));
                }
                Err(ParseFloatError::Empty(span)) => {
                    return Err(ParseRealError::Empty(span));
                }
                Err(ParseFloatError::NotFloat(_)) => {}
            }
        }
        match check_rational(&input) {
            Ok((rational, next)) => Ok((Real::Rational(rational), next)),
            Err(ParseRationalError::ParseSignError(err @ ParseSignError::UnknownPunct(_, _))) => {
                Err(ParseRealError::ParseSignError(err))
            }
            Err(ParseRationalError::ParseSignError(ParseSignError::MissingNumber(span))) => {
                Err(ParseRealError::Empty(span))
            }
            Err(ParseRationalError::ParseIntError(ParseIntError::NotInt(span))) => {
                Err(ParseRealError::NotReal(span))
            }
            Err(
                ParseRationalError::ParseIntError(ParseIntError::Empty(span))
                | ParseRationalError::Empty(span),
            ) => Err(ParseRealError::Empty(span)),
            Err(err @ ParseRationalError::MissingDenominator(_)) => {
                Err(ParseRealError::ParseRationalError(err))
            }
        }
    }

    #[derive(Debug, Error, CrispParserError)]
    pub enum ParseComplexError {
        #[error("{0}")]
        ParseSignError(#[from] ParseSignError),
        #[error("{0}")]
        ParseRationalError(#[from] ParseRationalError),
        #[error("expected '('")]
        MissingParentheses(Span),
        #[error("expected real part and imaginary part to be of the same type")]
        DifferentTypes(Span),
        #[error("expected real and imaginary values")]
        NoRealOrImaginary(Span),
        #[error("expected complex literal")]
        NotComplex(Span),
        #[error("unexpectedly reached end of input")]
        Empty(Span),
    }

    impl From<ParseRealError> for ParseComplexError {
        fn from(value: ParseRealError) -> Self {
            match value {
                ParseRealError::ParseSignError(err) => ParseComplexError::ParseSignError(err),
                ParseRealError::ParseRationalError(err) => {
                    ParseComplexError::ParseRationalError(err)
                }
                ParseRealError::NotReal(span) => ParseComplexError::NotComplex(span),
                ParseRealError::Empty(span) => ParseComplexError::Empty(span),
            }
        }
    }

    pub fn check_complex<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> Result<(Complex, syn::buffer::Cursor<'a>), ParseComplexError> {
        if let Some((punct, next)) = input.punct() {
            if punct.as_char() != '#' {
                return Err(ParseComplexError::NotComplex(punct.span()));
            };
            let next = if let Some((_, next2)) = next.ident().filter(|(ident, _)| ident == "C") {
                next2
            } else {
                return Err(ParseComplexError::NotComplex(next.span()));
            };
            if let Some((inner, inner_span, next2)) = next.group(Delimiter::Parenthesis) {
                if inner.eof() {
                    return Err(ParseComplexError::NoRealOrImaginary(
                        syn::spanned::Spanned::span(&inner_span),
                    ));
                }
                let (real, inner2) = check_real(&inner)?;
                if inner2.eof() {
                    return Err(ParseComplexError::NoRealOrImaginary(inner2.span()));
                }
                match check_real(&inner2)? {
                    (
                        Real::Float {
                            negative: imaginary_negative,
                            number: imaginary,
                        },
                        next3,
                    ) => {
                        if !next3.eof() {
                            return Err(ParseComplexError::NoRealOrImaginary(next3.span()));
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
                                Err(ParseComplexError::DifferentTypes(inner2.span()))
                            }
                        }
                    }
                    (Real::Rational(imaginary), next3) => {
                        if !next3.eof() {
                            return Err(ParseComplexError::NoRealOrImaginary(next3.span()));
                        }
                        match real {
                            Real::Float { .. } => {
                                Err(ParseComplexError::DifferentTypes(inner2.span()))
                            }
                            Real::Rational(real) => {
                                Ok((Complex::Rational { real, imaginary }, next2))
                            }
                        }
                    }
                }
            } else {
                Err(ParseComplexError::MissingParentheses(next.span()))
            }
        } else if input.eof() {
            Err(ParseComplexError::Empty(input.span()))
        } else {
            Err(ParseComplexError::NotComplex(input.span()))
        }
    }

    #[derive(Debug, Error, CrispParserError)]
    pub enum ParseNumberError {
        #[error("{0}")]
        ParseSignError(#[from] ParseSignError),
        #[error("{0}")]
        ParseRationalError(#[from] ParseRationalError),
        #[error("{0}")]
        ParseComplexError(#[from] ParseComplexError),
        #[error("expected number literal")]
        NotNumber(Span),
        #[error("unexpectedly reached end of input")]
        Empty(Span),
    }

    pub fn check_number<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> Result<(Number, syn::buffer::Cursor<'a>), ParseNumberError> {
        let input = *input;
        if let Some((complex, next)) = match check_complex(&input) {
            Ok(ret) => Ok(Some(ret)),
            Err(ParseComplexError::NotComplex(_)) => Ok(None),
            Err(ParseComplexError::Empty(span)) => Err(ParseNumberError::Empty(span)),
            Err(
                err @ ParseComplexError::DifferentTypes(_)
                | err @ ParseComplexError::MissingParentheses(_)
                | err @ ParseComplexError::ParseSignError(_)
                | err @ ParseComplexError::ParseRationalError(_)
                | err @ ParseComplexError::NoRealOrImaginary(_),
            ) => Err(ParseNumberError::ParseComplexError(err)),
        }? {
            Ok((Number::Complex(complex), next))
        } else {
            match check_real(&input) {
                Ok((real, next)) => Ok((Number::Real(real), next)),
                Err(ParseRealError::Empty(span)) => Err(ParseNumberError::Empty(span)),
                Err(
                    ParseRealError::NotReal(span)
                    | ParseRealError::ParseSignError(ParseSignError::UnknownPunct(span, _)),
                ) => Err(ParseNumberError::NotNumber(span)),
                Err(ParseRealError::ParseRationalError(err)) => {
                    Err(ParseNumberError::ParseRationalError(err))
                }
                Err(ParseRealError::ParseSignError(err)) => {
                    Err(ParseNumberError::ParseSignError(err))
                }
            }
        }
    }

    #[derive(Debug, Error, CrispParserError)]
    pub enum ParseSymbolError {
        #[error("expected symbol")]
        NotSymbol(Span),
        #[error("unexpectedly reached end of input")]
        Empty(Span),
    }

    pub fn check_symbol<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> Result<(Symbol, syn::buffer::Cursor<'a>), ParseSymbolError> {
        if input.eof() {
            Err(ParseSymbolError::Empty(input.span()))
        } else if let Some((ident, next)) = input.ident() {
            Ok((Symbol { ident }, next))
        } else {
            Err(ParseSymbolError::NotSymbol(input.span()))
        }
    }

    #[derive(Debug, Error, CrispParserError)]
    pub enum ParseTokenError {
        #[error("{0}")]
        ParseNumberError(#[from] ParseNumberError),
        #[error("expected crisp token")]
        NotToken(Span),
        #[error("unexpectedly reached end of input")]
        Empty(Span),
    }

    pub fn check_token<'a, C: Deref<Target = syn::buffer::Cursor<'a>>>(
        input: C,
    ) -> Result<(CrispToken, syn::buffer::Cursor<'a>), ParseTokenError> {
        let input = *input;
        if let Some((number, next)) = match check_number(&input) {
            Ok(ret) => Ok(Some(ret)),
            Err(ParseNumberError::Empty(span)) => Err(ParseTokenError::Empty(span)),
            Err(ParseNumberError::NotNumber(_)) => Ok(None),
            Err(
                err @ ParseNumberError::ParseSignError(_)
                | err @ ParseNumberError::ParseRationalError(_)
                | err @ ParseNumberError::ParseComplexError(_),
            ) => Err(ParseTokenError::ParseNumberError(err)),
        }? {
            Ok((CrispToken::Number(number), next))
        } else if let Some((symbol, next)) = match check_symbol(&input) {
            Ok(ret) => Ok(Some(ret)),
            Err(ParseSymbolError::Empty(span)) => Err(ParseTokenError::Empty(span)),
            Err(ParseSymbolError::NotSymbol(_)) => Ok(None),
        }? {
            Ok((CrispToken::Symbol(symbol), next))
        } else {
            Err(ParseTokenError::NotToken(input.span()))
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
        input.step(|cursor| check_rational(cursor).map_err(|err| err.into()))
    }
}

impl ToTokens for Rational {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let is_negative = self.is_negative();
        match self {
            Rational::Integer { number, .. } => {
                let formatted = number.to_string();
                if is_negative {
                    tokens.append_all(quote! {::core::ops::Neg::neg(<::crisp::num::BigInt as ::core::str::FromStr>::from_str(#formatted).unwrap())})
                } else {
                    tokens.append_all(quote! {<::crisp::num::BigInt as ::core::str::FromStr>::from_str(#formatted).unwrap()})
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
                    tokens.append_all(quote! {::crisp::num::BigRational::new(::core::ops::Neg::neg(<::crisp::num::BigInt as ::core::str::FromStr>::from_str(#formatted_numerator).unwrap()),<::crisp::num::BigInt as ::core::str::FromStr>::from_str(#formatted_denominator).unwrap())});
                } else {
                    tokens.append_all(quote! {::crisp::num::BigRational::new(<::crisp::num::BigInt as ::core::str::FromStr>::from_str(#formatted_numerator).unwrap(),<::crisp::num::BigInt as ::core::str::FromStr>::from_str(#formatted_denominator).unwrap())});
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
        input.step(|cursor| check_real(cursor).map_err(|err| err.into()))
    }
}

impl ToTokens for Real {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let is_negative = self.is_negative();
        match self {
            Real::Float { number, .. } => {
                let formatted = format!("{}{}", if is_negative { "-" } else { "" }, number);
                tokens.append_all(quote! {<::crisp::num::BigFloat as ::core::str::FromStr>::from_str(#formatted).unwrap()});
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
        input.step(|cursor| check_complex(cursor).map_err(|err| err.into()))
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
                let mut token_stream_real = TokenStream::new();
                let mut token_stream_imaginary = TokenStream::new();
                token_stream_real.append_all(quote! {<::crisp::num::BigFloat as ::core::str::FromStr>::from_str(#formatted_real).unwrap()});
                token_stream_imaginary.append_all(quote! {<::crisp::num::BigFloat as ::core::str::FromStr>::from_str(#formatted_imaginary).unwrap()});
                if let Some(true) = real_negative {
                    token_stream_real.append_all(quote! {.neg()});
                }
                if let Some(true) = imaginary_negative {
                    token_stream_imaginary.append_all(quote! {.neg()});
                }
                tokens.append_all(
                    quote! {::crisp::num::Complex::new(#token_stream_real,#token_stream_imaginary)},
                )
            }
            Complex::Rational { real, imaginary } => {
                tokens.append_all(quote! {::crisp::num::Complex::new(#real,#imaginary)})
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
        input.step(|cursor| check_number(cursor).map_err(|err| err.into()))
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
        input.step(|cursor| check_symbol(cursor).map_err(|err| err.into()))
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
        println!("{:?}", input);
        input.step(|cursor| check_token(cursor).map_err(|err| err.into()))
    }
}

impl ToTokens for CrispToken {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            CrispToken::Number(number) => {
                number.to_tokens(tokens);
            }
            CrispToken::Symbol(symbol) => {
                symbol.to_tokens(tokens);
            }
        }
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
        syn::parse_str::<CrispToken>("#C(-2 1)").expect("Not a number");
        syn::parse_str::<CrispToken>("#C(-5/9 -1/3)").expect("Not a number");
        syn::parse_str::<CrispToken>("#C(2e3 27e10)").expect("Not a number");
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

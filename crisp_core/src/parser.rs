use crate::parser::_inner::*;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::{LitFloat, LitInt};

pub(self) mod _inner {
    use crate::parser::{Complex, CrispToken, Number, Rational, Real, Symbol};
    use core::fmt::{Debug, Formatter};
    use core::ops::Deref;
    use proc_macro2::{Delimiter, Span, TokenTree};
    use syn::{LitFloat, LitInt};

    #[derive(Debug)]
    pub enum ParseIntError {
        NotInt(Span),
        Empty(Span),
    }

    impl ParseIntError {
        fn span(&self) -> Span {
            match self {
                ParseIntError::NotInt(span) => span.to_owned(),
                ParseIntError::Empty(span) => span.to_owned(),
            }
        }
    }

    impl core::fmt::Display for ParseIntError {
        fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
            match self {
                ParseIntError::NotInt(_) => write!(f, "expected an integer"),
                ParseIntError::Empty(_) => write!(f, "unexpectedly reached end of input"),
            }
        }
    }

    impl core::error::Error for ParseIntError {}

    impl From<ParseIntError> for syn::Error {
        fn from(value: ParseIntError) -> Self {
            match value {
                ParseIntError::NotInt(span) | ParseIntError::Empty(span) => {
                    syn::Error::new(span, value)
                }
            }
        }
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

    #[derive(Debug)]
    pub enum ParseFloatError {
        NotFloat(Span),
        Empty(Span),
    }

    impl ParseFloatError {
        fn span(&self) -> Span {
            match self {
                ParseFloatError::NotFloat(span) => span.to_owned(),
                ParseFloatError::Empty(span) => span.to_owned(),
            }
        }
    }

    impl core::fmt::Display for ParseFloatError {
        fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
            match self {
                ParseFloatError::NotFloat(_) => write!(f, "expected a float"),
                ParseFloatError::Empty(_) => write!(f, "unexpectedly reached end of input"),
            }
        }
    }

    impl core::error::Error for ParseFloatError {}

    impl From<ParseFloatError> for syn::Error {
        fn from(value: ParseFloatError) -> Self {
            match value {
                ParseFloatError::NotFloat(span) | ParseFloatError::Empty(span) => {
                    syn::Error::new(span, value)
                }
            }
        }
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

    #[derive(Debug)]
    pub enum ParseSignError {
        // Case where there is a punctuation, but it not either of '+' or '-'.
        UnknownPunct(Span, char),
        // Case where we have the right punctuation ('+' or '-'), but there is no following token.
        MissingNumber(Span),
    }

    impl ParseSignError {
        fn span(&self) -> Span {
            match self {
                ParseSignError::UnknownPunct(span, _) => span.to_owned(),
                ParseSignError::MissingNumber(span) => span.to_owned(),
            }
        }
    }

    impl core::fmt::Display for ParseSignError {
        fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
            match self {
                ParseSignError::MissingNumber(_) => {
                    write!(f, "expected a number literal after the sign")
                }
                ParseSignError::UnknownPunct(_, punct) => {
                    write!(f, "expected '+' or '-' before the number, got '{}'", punct)
                }
            }
        }
    }

    impl core::error::Error for ParseSignError {}

    impl From<ParseSignError> for syn::Error {
        fn from(value: ParseSignError) -> Self {
            match value {
                ParseSignError::MissingNumber(span) | ParseSignError::UnknownPunct(span, _) => {
                    syn::Error::new(span, value)
                }
            }
        }
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

    #[derive(Debug)]
    pub enum ParseRationalError {
        ParseSignError(ParseSignError),
        ParseIntError(ParseIntError),
        MissingDenominator(Span),
        Empty(Span),
    }

    impl ParseRationalError {
        fn span(&self) -> Span {
            match self {
                ParseRationalError::ParseSignError(err) => err.span(),
                ParseRationalError::ParseIntError(err) => err.span(),
                ParseRationalError::MissingDenominator(span) => span.to_owned(),
                ParseRationalError::Empty(span) => span.to_owned(),
            }
        }
    }

    impl core::fmt::Display for ParseRationalError {
        fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
            match self {
                ParseRationalError::ParseSignError(err) => core::fmt::Display::fmt(&err, f),
                ParseRationalError::ParseIntError(err) => core::fmt::Display::fmt(&err, f),
                ParseRationalError::MissingDenominator(_) => write!(f, "missing denominator"),
                ParseRationalError::Empty(_) => write!(f, "unexpectedly reached end of input"),
            }
        }
    }

    impl core::error::Error for ParseRationalError {}

    impl From<ParseRationalError> for syn::Error {
        fn from(value: ParseRationalError) -> Self {
            match &value {
                ParseRationalError::ParseSignError(err) => syn::Error::new(err.span(), value),
                ParseRationalError::ParseIntError(err) => syn::Error::new(err.span(), value),
                ParseRationalError::MissingDenominator(span) => {
                    syn::Error::new(span.to_owned(), value)
                }
                ParseRationalError::Empty(span) => syn::Error::new(span.to_owned(), value),
            }
        }
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

    #[derive(Debug)]
    pub enum ParseRealError {
        ParseSignError(ParseSignError),
        ParseRationalError(ParseRationalError),
        NotReal(Span),
        Empty(Span),
    }

    impl ParseRealError {
        fn span(&self) -> Span {
            match self {
                ParseRealError::ParseSignError(err) => err.span(),
                ParseRealError::ParseRationalError(err) => err.span(),
                ParseRealError::NotReal(span) => span.to_owned(),
                ParseRealError::Empty(span) => span.to_owned(),
            }
        }
    }

    impl core::fmt::Display for ParseRealError {
        fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
            match self {
                ParseRealError::ParseSignError(err) => core::fmt::Display::fmt(&err, f),
                ParseRealError::ParseRationalError(err) => core::fmt::Display::fmt(&err, f),
                ParseRealError::NotReal(_) => write!(f, "expected real literal"),
                ParseRealError::Empty(_) => write!(f, "unexpectedly reached end of input"),
            }
        }
    }

    impl core::error::Error for ParseRealError {}

    impl From<ParseRealError> for syn::Error {
        fn from(value: ParseRealError) -> Self {
            match &value {
                ParseRealError::ParseSignError(err) => syn::Error::new(err.span(), value),
                ParseRealError::ParseRationalError(err) => syn::Error::new(err.span(), value),
                ParseRealError::NotReal(span) => syn::Error::new(span.to_owned(), value),
                ParseRealError::Empty(span) => syn::Error::new(span.to_owned(), value),
            }
        }
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

    #[derive(Debug)]
    pub enum ParseComplexError {
        ParseSignError(ParseSignError),
        ParseRationalError(ParseRationalError),
        MissingParentheses(Span),
        DifferentTypes(Span),
        NoRealOrImaginary(Span),
        NotComplex(Span),
        Empty(Span),
    }

    impl ParseComplexError {
        fn span(&self) -> Span {
            match self {
                ParseComplexError::ParseSignError(err) => err.span(),
                ParseComplexError::ParseRationalError(err) => err.span(),
                ParseComplexError::MissingParentheses(span) => span.to_owned(),
                ParseComplexError::DifferentTypes(span) => span.to_owned(),
                ParseComplexError::NoRealOrImaginary(span) => span.to_owned(),
                ParseComplexError::NotComplex(span) => span.to_owned(),
                ParseComplexError::Empty(span) => span.to_owned(),
            }
        }
    }

    impl core::fmt::Display for ParseComplexError {
        fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
            match self {
                ParseComplexError::ParseSignError(err) => core::fmt::Display::fmt(&err, f),
                ParseComplexError::ParseRationalError(err) => core::fmt::Display::fmt(&err, f),
                ParseComplexError::MissingParentheses(_) => write!(f, "expected '('"),
                ParseComplexError::DifferentTypes(_) => write!(
                    f,
                    "expected real part and imaginary part to be of the same type"
                ),
                ParseComplexError::NoRealOrImaginary(_) => {
                    write!(f, "expected real and imaginary values")
                }
                ParseComplexError::NotComplex(_) => write!(f, "expected complex literal"),
                ParseComplexError::Empty(_) => write!(f, "unexpectedly reached end of input"),
            }
        }
    }

    impl core::error::Error for ParseComplexError {}

    impl From<ParseComplexError> for syn::Error {
        fn from(value: ParseComplexError) -> Self {
            match &value {
                ParseComplexError::ParseSignError(err) => syn::Error::new(err.span(), value),
                ParseComplexError::ParseRationalError(err) => syn::Error::new(err.span(), value),
                ParseComplexError::MissingParentheses(span) => {
                    syn::Error::new(span.to_owned(), value)
                }
                ParseComplexError::DifferentTypes(span) => syn::Error::new(span.to_owned(), value),
                ParseComplexError::NoRealOrImaginary(span) => {
                    syn::Error::new(span.to_owned(), value)
                }
                ParseComplexError::NotComplex(span) => syn::Error::new(span.to_owned(), value),
                ParseComplexError::Empty(span) => syn::Error::new(span.to_owned(), value),
            }
        }
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
                    return Err(ParseComplexError::NoRealOrImaginary(syn::spanned::Spanned::span(&inner_span)));
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
        } else {
            Err(ParseComplexError::Empty(input.span()))
        }
    }

    #[derive(Debug)]
    pub enum ParseNumberError {
        ParseSignError(ParseSignError),
        ParseRationalError(ParseRationalError),
        ParseComplexError(ParseComplexError),
        NotNumber(Span),
        Empty(Span),
    }

    impl ParseNumberError {
        fn span(&self) -> Span {
            match self {
                ParseNumberError::ParseSignError(err) => err.span(),
                ParseNumberError::ParseRationalError(err) => err.span(),
                ParseNumberError::ParseComplexError(err) => err.span(),
                ParseNumberError::NotNumber(span) => span.to_owned(),
                ParseNumberError::Empty(span) => span.to_owned(),
            }
        }
    }

    impl core::fmt::Display for ParseNumberError {
        fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
            match self {
                ParseNumberError::ParseSignError(err) => core::fmt::Display::fmt(&err, f),
                ParseNumberError::ParseRationalError(err) => core::fmt::Display::fmt(&err, f),
                ParseNumberError::ParseComplexError(err) => core::fmt::Display::fmt(&err, f),
                ParseNumberError::NotNumber(_) => write!(f, "expected number literal"),
                ParseNumberError::Empty(_) => write!(f, "unexpectedly reached end of input"),
            }
        }
    }

    impl core::error::Error for ParseNumberError {}

    impl From<ParseNumberError> for syn::Error {
        fn from(value: ParseNumberError) -> Self {
            match &value {
                ParseNumberError::ParseSignError(err) => syn::Error::new(err.span(), value),
                ParseNumberError::ParseRationalError(err) => syn::Error::new(err.span(), value),
                ParseNumberError::ParseComplexError(err) => syn::Error::new(err.span(), value),
                ParseNumberError::NotNumber(span) => syn::Error::new(span.to_owned(), value),
                ParseNumberError::Empty(span) => syn::Error::new(span.to_owned(), value),
            }
        }
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

    #[derive(Debug)]
    pub enum ParseSymbolError {
        NotSymbol(Span),
        Empty(Span),
    }

    impl ParseSymbolError {
        fn span(&self) -> Span {
            match self {
                ParseSymbolError::NotSymbol(span) => span.to_owned(),
                ParseSymbolError::Empty(span) => span.to_owned(),
            }
        }
    }

    impl core::fmt::Display for ParseSymbolError {
        fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
            match self {
                ParseSymbolError::NotSymbol(_) => write!(f, "expected symbol"),
                ParseSymbolError::Empty(_) => write!(f, "unexpectedly reached end of input"),
            }
        }
    }

    impl core::error::Error for ParseSymbolError {}

    impl From<ParseSymbolError> for syn::Error {
        fn from(value: ParseSymbolError) -> Self {
            match &value {
                ParseSymbolError::NotSymbol(span) => syn::Error::new(span.to_owned(), value),
                ParseSymbolError::Empty(span) => syn::Error::new(span.to_owned(), value),
            }
        }
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

    #[derive(Debug)]
    pub enum ParseTokenError {
        ParseNumberError(ParseNumberError),
        NotToken(Span),
        Empty(Span),
    }

    impl ParseTokenError {
        fn span(&self) -> Span {
            match self {
                ParseTokenError::ParseNumberError(err) => err.span(),
                ParseTokenError::NotToken(span) => span.to_owned(),
                ParseTokenError::Empty(span) => span.to_owned(),
            }
        }
    }

    impl core::fmt::Display for ParseTokenError {
        fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
            match self {
                ParseTokenError::ParseNumberError(err) => core::fmt::Display::fmt(&err, f),
                ParseTokenError::NotToken(_) => write!(f, "expected crisp token"),
                ParseTokenError::Empty(_) => write!(f, "unexpectedly reached end of input"),
            }
        }
    }

    impl core::error::Error for ParseTokenError {}

    impl From<ParseTokenError> for syn::Error {
        fn from(value: ParseTokenError) -> Self {
            match &value {
                ParseTokenError::ParseNumberError(err) => syn::Error::new(err.span(), value),
                ParseTokenError::NotToken(span) => syn::Error::new(span.to_owned(), value),
                ParseTokenError::Empty(span) => syn::Error::new(span.to_owned(), value),
            }
        }
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
        input.step(|cursor| check_real(cursor).map_err(|err| err.into()))
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
                token_stream_real.append_all(quote! {<::crisp_core::num::BigFloat as ::core::str::FromStr>::from_str(#formatted_real).unwrap()});
                token_stream_imaginary.append_all(quote! {<::crisp_core::num::BigFloat as ::core::str::FromStr>::from_str(#formatted_imaginary).unwrap()});
                if let Some(true) = real_negative {
                    token_stream_real.append_all(quote! {.neg()});
                }
                if let Some(true) = imaginary_negative {
                    token_stream_imaginary.append_all(quote! {.neg()});
                }
                tokens.append_all(quote! {::crisp_core::num::Complex::new(#token_stream_real,#token_stream_imaginary)})
            }
            Complex::Rational { real, imaginary } => {
                tokens.append_all(quote! {::crisp_core::num::Complex::new(#real,#imaginary)})
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

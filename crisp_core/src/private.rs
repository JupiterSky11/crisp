pub trait Spanned {
    fn span(&self) -> ::proc_macro2::Span;
}

pub struct SynError {
    inner: ::syn::Error,
}

impl<T: Spanned + ::core::fmt::Display> From<T> for SynError {
    fn from(value: T) -> Self {
        Self {
            inner: syn::Error::new(value.span(), value.to_string()),
        }
    }
}

impl From<SynError> for ::syn::Error {
    fn from(value: SynError) -> Self {
        value.inner
    }
}

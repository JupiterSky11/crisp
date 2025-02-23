use crisp_core::Crisp;
use quote::quote;
use syn::parse_macro_input;

// #[proc_macro]
// pub fn eval(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
//     4
// }

#[proc_macro]
pub fn eval(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    println!("Parsing input as ParseStream");
    let crisp: Crisp = parse_macro_input!(input as Crisp);
    println!("Evaluating...");
    todo!()
    // crisp.eval().to_rust().into()
}

#[proc_macro]
pub fn crisp_token(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let x: crisp_core::parser::CrispToken =
        parse_macro_input!(input as crisp_core::parser::CrispToken);
    quote!(#x).into()
}

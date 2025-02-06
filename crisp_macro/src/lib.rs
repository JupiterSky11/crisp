use syn::parse_macro_input;
use crisp_core::Crisp;

// #[proc_macro]
// pub fn eval(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
//     4
// }

#[proc_macro]
pub fn eval(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    println!("Parsing input as ParseStream");
    let crisp: Crisp = parse_macro_input!(input as Crisp);
    println!("Evaluating...");
    crisp.eval().to_rust().into()
}

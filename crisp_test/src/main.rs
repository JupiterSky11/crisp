extern crate crisp_core;

use std::any::type_name_of_val;

fn main() {
    let x = crisp_macro::crisp_token! {#C(-2/3 2/4)};
    println!("{}", x);
    println!("{}", type_name_of_val(&x));
    let G = crisp_macro::crisp_token! {+999999999999999999999999999999999999999999000000000000088888888888888888888888888888888888888888888888888888888999999999999999999999999};
    println!("{}", G);
    let G = crisp_macro::crisp_token! {+9.99999999999999999999999999999777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777666666666666666666666777777777777777777777e100000000};
    println!("{}", G);
    println!("{}", type_name_of_val(&G));
}

#[cfg(test)]
mod tests {

    use std::any::type_name_of_val;
    use std::ops::Neg;

    #[test]
    fn it_works() {
        let x = crisp_macro::crisp_token! {#C(-2/3 2/4)};
        let G = crisp_macro::crisp_token! {+999999999999999999999999999999999999999999000000000000088888888888888888888888888888888888888888888888888888888999999999999999999999999};
        let G = crisp_macro::crisp_token! {+9.99999999999999999999999999999777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777666666666666666666666777777777777777777777e+100000000};
    }
}

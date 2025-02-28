extern crate crisp;

use std::any::type_name_of_val;

fn main() {
    let x = crisp::crisp_token! {#C(-2/3 2/4)};
    println!("{}", x);
    println!("{}", type_name_of_val(&x));
    let x = crisp::crisp_token! {#C(0.1 -0.2)};
    println!("{:?}", x);
    let G = crisp::crisp_token! {+999999999999999999999999999999999999999999000000000000088888888888888888888888888888888888888888888888888888888999999999999999999999999};
    println!("{}", G);
    let G = crisp::crisp_token! {+9.99999999999999999999999999999777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777666666666666666666666777777777777777777777e100000000};
    println!("{}", G);
    println!("{}", type_name_of_val(&G));

    // A rational has to be two integers
    let num = crisp::crisp_token! {#C(2/1 0/1)};
    println!("{}", num);
    let x = crisp::crisp_token! {1};
    println!("{}", x);
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        crisp::crisp_token! {#C(-2/3 2/4)};
        crisp::crisp_token! {#C(0.1 -0.2)};
        crisp::crisp_token! {+999999999999999999999999999999999999999999000000000000088888888888888888888888888888888888888888888888888888888999999999999999999999999};
        crisp::crisp_token! {+9.99999999999999999999999999999777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777666666666666666666666777777777777777777777e+100000000};
        crisp::crisp_token! {1};
        crisp::crisp_token! {+0};
        crisp::crisp_token! {#C(1 2)};
    }
}

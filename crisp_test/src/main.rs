extern crate crisp_core;

fn main() {
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let x = crisp_macro::crisp_token!{#C(-2/3 2/4)};
        println!("{}", x);
        let G = crisp_macro::crisp_token!(+999999999999999999999999999999999999999999000000000000088888888888888888888888888888888888888888888888888888888999999999999999999999999);
        println!("{}", G);
        // let G = crisp_macro::crisp_token!(+9.9999999999999999999999999999e10000000000000000);
        // println!("{}", G);
    }
}

use crisp_api::*;
fn main() {
    let foo = eval!(+ (+ 2 2) (+ -1 16));
    if let CrispType::Int(x) = foo {
        println!("Hello, world! {}", x);
    } else {
        println!("Hello, world!  (Crisp failure.)");
    }
}

#[cfg(test)]
mod tests {
    use crisp_api::*;

    #[test]
    fn it_works() {
        let mut result: i32 = 0;
        if let CrispType::Int(x) = eval!(+ (+ -1 3) (+ 2 (+ 2 2))) {
            result = x;
        }
        assert_eq!(result, 8);
    }
}

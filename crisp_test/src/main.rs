use crisp_macro::eval;
use crisp_core::CrispType;
fn main() {
    if let CrispType::Int(x) = eval!(+ 0 0) {
        println!("Hello, world! {}", x);
    } else {
        println!("Hello, world!  (Crisp failure.)")
    }
}

#[cfg(test)]
mod tests {
    use crisp_macro::eval;
    use crisp_core::CrispType;

    #[test]
    fn it_works() {
        let mut result: i32 = 0;
        if let CrispType::Int(x) = eval!(+ (+ -1 3) (+ 2 (+ 2 2))) {
            result = x;
        }
        assert_eq!(result, 8);
    }
}




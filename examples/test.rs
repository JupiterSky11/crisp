use std::{any::type_name_of_val};

fn main() {
    let x = crisp::parse_token! {#C(-2/3 2/4)};
    println!("{}", x);
    println!("{}", type_name_of_val(&x));
    let x = crisp::parse_token! {#C(0.1 -0.2)};
    println!("{:?}", x);
    let G = crisp::parse_token! {+999999999999999999999999999999999999999999000000000000088888888888888888888888888888888888888888888888888888888999999999999999999999999};
    println!("{}", G);
    let G = crisp::parse_token! {+9.99999999999999999999999999999777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777666666666666666666666777777777777777777777e100000000};
    println!("{}", G);
    println!("{}", type_name_of_val(&G));

    // A rational has to be two integers
    let num = crisp::parse_token! {#C(2/1 0/1)};
    println!("{}", num);
    let x = crisp::parse_token! {1};
    println!("{}", x);
    crisp::parse_token!(x);
    crisp::parse_expr!(-1);
    crisp::parse_expr!(+2);
    let x = 2;
    crisp::parse_expr!((drop x));
}



#[cfg(test)]
mod tests {
    use crate::hello_crisp;
    #[test]
    fn it_works() {
        crisp::parse_token! {#C(-2/3 2/4)};
        crisp::parse_token! {#C(0.1 -0.2)};
        crisp::parse_token! {+999999999999999999999999999999999999999999000000000000088888888888888888888888888888888888888888888888888888888999999999999999999999999};
        crisp::parse_token! {+9.99999999999999999999999999999777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777666666666666666666666777777777777777777777e+100000000};
        crisp::parse_token! {1};
        crisp::parse_token! {+0};
        crisp::parse_token! {#C(1 2)};
        crisp::parse_expr! {(drop 4/3)};
        crisp::parse_expr! {()};
        crisp::parse_expr! {#C(1 0)};

        hello_crisp();
    }
}






fn hello_crisp() {
    let mut foo: List = vec![];
    let crisped_addition = get_crisp_function();
    foo.push(Atom { vtype: CrispType::Fn(crisped_addition) });
    foo.push((21 as i32).crisp());
    foo.push((21 as i32).crisp());
    let bar: Atom = SExpr::from_list(foo).unwrap().eval().unwrap();
    if let CrispType::Number(x) = bar.vtype { println!("{}", x) };
}



// --- Crisp generalization core

// This guarantees a given external type has a Crisp equivalent
trait Crispy {
    fn crisp(&self) -> Atom;
}

use std::rc::Rc;
#[derive(Clone)]
struct Atom {
    vtype: CrispType,
}

type Number = Rc<i32>;
type List = Vec<Atom>;
#[derive(Clone)]
enum CrispType {
    List(List),
    Fn(CrispFunction),
    Number(Number),
}

impl Crispy for i32 {
    fn crisp(&self) -> Atom {
        Atom { vtype: CrispType::Number(Rc::new(self.clone())) }
    }
}

struct SExpr {
    func: CrispFunction,
    args: List,
}

impl SExpr {
    fn eval(&self) -> Result<Atom, String> {
        self.func.call(self.args.clone())
    }
    fn from_list(list: List) -> Result<SExpr, ()> {
        if let CrispType::Fn(func) = &list[0].vtype {
            let args: List = list[1..].to_vec();
            Ok(SExpr {
                func: func.clone(),
                args,
            })
        } else { Err(()) }
    }
}

// This will be used in the future to dynamically check the signature before passing a list of
// arguments to be called by the specified function.
#[derive(Clone)]
struct CrispFunction {
    function: Rc<dyn CrispyFn>,
    signature: CrispSignature,
}

impl CrispFunction {
    fn call(&self, args: List) -> Result<Atom, String> {
        self.function.call(args)
    }
}

// Same deal as above, basically useless for the time being.
#[derive(Clone)]
struct CrispSignature {
    builder: bool,
    args: Rc<List>,
    output: Rc<CrispType>,
}

// This guarantees that a struct will act as a function, and has a call method.
trait CrispyFn {
    fn new() -> Self where Self: Sized;
    fn call(&self, args: List) -> Result<Atom, String>;
}



// --- Test functions for manually creating Crisp functions

// This just creates a CrispFunction for the hello_crisp test.
fn get_crisp_function() -> CrispFunction {
    CrispFunction {
        function: Rc::new(CrispFunctionAdd {}),
        signature: CrispSignature {
            builder: false,
            args: List::new().into(),
            output: Rc::new(CrispType::Number(Rc::new(0))),
        }
    }
}

// This is an example for how to implement an external function.  
// This struct bridges a list of CrispType arguments into the function call.
// We will need one of these for every function, and we will need a macro to generate them.
struct CrispFunctionAdd {}
impl CrispyFn for CrispFunctionAdd {
    fn new() -> CrispFunctionAdd { CrispFunctionAdd {} }
    fn call(&self, args: List) -> Result<Atom, String> {
        if let CrispType::Number(x) = &args[0].vtype {
            if let CrispType::Number(y) = &args[1].vtype {
                Ok(Atom {
                    // Obviously here I just call the normal + operator, but this would be some
                    // external function otherwise.
                    vtype: CrispType::Number((**x + **y).into())
                })
            } else { return Err("type invalid, second argument".to_string()) }
        } else { return Err("type invalid, first argument".to_string()) }
    }
}


// CrispFn trait with call(&self, List) method
// New struct for each individual external function
// Implements call() method to wire the args into the inputs.

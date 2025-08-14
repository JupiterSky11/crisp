#![allow(dead_code)]
use std::{any::type_name_of_val};

fn main() {
    let x = crisp::parse_token! {#C(-2/3 2/4)};
    println!("{}", x);
    println!("{}", type_name_of_val(&x));
    let x = crisp::parse_token! {#C(0.1 -0.2)};
    println!("{:?}", x);
    let g = crisp::parse_token! {+999999999999999999999999999999999999999999000000000000088888888888888888888888888888888888888888888888888888888999999999999999999999999};
    println!("{}", g);
    let g = crisp::parse_token! {+9.99999999999999999999999999999777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777666666666666666666666777777777777777777777e100000000};
    println!("{}", g);
    println!("{}", type_name_of_val(&g));

    // A rational has to be two integers
    let num = crisp::parse_token! {#C(2/1 0/1)};
    println!("{}", num);
    let x = crisp::parse_token! {1};
    println!("{}", x);
    _ = crisp::parse_token!(x);
    _ = crisp::parse_expr!(-1);
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
        let args: Vec<String> = std::env::args().collect();
        if crate::does_arg(args, "--repl") {
            crate::repl();
        }
    }
}





fn hello_crisp() {
    let mut foo: List = vec![];
    foo.push(CrispFunctionAdd::new().atomize());
    foo.push((21 as i32).crisp());
    foo.push((21 as i32).crisp());
    let bar: Atom = SExpr::from_list(foo).unwrap().eval().unwrap();
    if let CrispType::Number(x) = bar.data { println!("{}", x) };
    parser::parse("eval (println \"Some day...\") (println \"We will finish Crisp.\")");
}

fn repl() {
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input);
        parser::parse(&input);
    }
}

fn does_arg(args: Vec<String>, s: &str) -> bool {
    for a in args {
        if a == s {
            return true
        }
    }
    false
}

// --- Crisp generalization core

// This guarantees a given external type has a Crisp equivalent
trait Crispy {
    fn crisp(&self) -> Atom;
}

use std::rc::Rc;
#[derive(Clone)]
struct Atom {
    data: CrispType,
    symbol: sym::Symbol,
}

type Number = Rc<i32>;
type List = Vec<Atom>;
impl From<List> for Atom {
    fn from(l: List) -> Self {
        Atom {
            data: CrispType::List(l),
            symbol: sym::Symbol::List,
        }
    }
}
#[derive(Clone)]
enum CrispType {
    List(List),
    Fn(CrispFunction),
    Number(Number),
    Symbol(String),
}

impl Crispy for i32 {
    fn crisp(&self) -> Atom {
        Atom { data: CrispType::Number(Rc::new(self.clone())), symbol: sym::Symbol::Struct( sym::Struct { name: "i32".to_string(), traits: vec!["Add".to_string()] })}
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
        if let CrispType::Fn(func) = &list.get(0).unwrap().data {
            let args: List = list[1..].to_vec();
            Ok(SExpr {
                func: func.clone(),
                args,
            })
        } else {
            println!("For some reason, the type is wronga");
            match &list.get(0).unwrap().data {
                CrispType::List(_) => println!("List!!!"),
                CrispType::Symbol(s) => println!("Symbol...? [{}]", s),
                CrispType::Fn(_) => println!("Fu... wha?"),
                _ => print!("sadness"),
            }
            Err(())
        }
    }
}

mod sym {
    #[derive(Clone, PartialEq)]
    pub enum Symbol {
        Unknown(String),
        Enum(Enum),
        Struct(Struct),
        Fn(Fn),
        List,
    }
    impl std::fmt::Display for Symbol {
        fn fmt(&self, f: &mut std::fmt::Formatter::<'_>) -> Result<(), std::fmt::Error> {
            write!(f, "{}", match self {
                Symbol::Unknown(s) => "Unknown: ".to_owned()+s,
                Symbol::List => "List".to_string(),
                Symbol::Fn(s) => "Fn: ".to_string()+&s.name,
                Symbol::Struct(s) => "Struct: ".to_string()+&s.name,
                Symbol::Enum(s) => "Enum: ".to_string()+&s.name,
            })
        }
    }

    #[derive(Clone, PartialEq)]
    pub struct Struct {
        pub name: String,
        pub traits: Vec<Trait>
    }
    #[derive(Clone, PartialEq)]
    pub struct Enum {
        pub name: String,
        pub variants: Vec<Variant>
    }
    #[derive(Clone, PartialEq)]
    pub struct Fn {
        pub name: String,
    }
    type Trait = String;
    type Variant = String;
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
    fn atomize(self) -> Atom;
    fn call(&self, args: List) -> Result<Atom, String>;
}



// --- Test functions for manually creating Crisp functions

// This just creates a CrispFunction for the hello_crisp test.
fn get_crisp_function(func: Rc<dyn CrispyFn>) -> CrispFunction {
    CrispFunction {
        function: func,
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
#[derive(Clone)]
struct CrispFunctionAdd {}
impl CrispyFn for CrispFunctionAdd {
    fn new() -> CrispFunctionAdd { CrispFunctionAdd {} }
    fn atomize(self) -> Atom {
        Atom {
            data: CrispType::Fn(get_crisp_function(Rc::new(self))),
            symbol: sym::Symbol::Fn(sym::Fn { name: "add".to_string() }),
        }
    }
    fn call(&self, args: List) -> Result<Atom, String> {
        if let CrispType::Number(x) = &args[0].data {
            if let CrispType::Number(y) = &args[1].data {
                Ok(Atom {
                    // Obviously here I just call the normal + operator, but this would be some
                    // external function otherwise.
                    data: CrispType::Number((**x + **y).into()),
                    symbol: sym::Symbol::Fn(sym::Fn {name: "add".to_string()}),
                })
            } else { return Err("type invalid, second argument".to_string()) }
        } else { return Err("type invalid, first argument".to_string()) }
    }
}

#[derive(Clone)]
struct CrispFunctionPrintln {}
impl CrispyFn for CrispFunctionPrintln {
    fn new() -> CrispFunctionPrintln { CrispFunctionPrintln {} }
    fn atomize(self) -> Atom {
        Atom {
            data: CrispType::Fn(get_crisp_function(Rc::new(self))),
            symbol: sym::Symbol::Fn(sym::Fn { name: "println".to_string() }),
        }
    }
    fn call(&self, args: List) -> Result<Atom, String> {
        if let Some(a1) = args.get(0) {
            if a1.symbol == sym_string() {
                if let CrispType::Symbol(b2) = &a1.data {
                    println!("{}", b2);
                }
            }
        }
        Ok(Atom { data: CrispType::List(List::new()), symbol: sym::Symbol::List})
    }
}

#[derive(Clone)]
struct CrispFunctionEval {}
impl CrispyFn for CrispFunctionEval {
    fn new() -> CrispFunctionEval { CrispFunctionEval {} }
    fn atomize(self) -> Atom {
        Atom {
            data: CrispType::Fn(get_crisp_function(Rc::new(self))),
            symbol: sym::Symbol::Fn(sym::Fn { name: "eval".to_string() }),
        }
    }
    fn call(&self, args: List) -> Result<Atom, String> {
        for a in &args {
            if let CrispType::List(b) = &a.data {
                SExpr::from_list(b.clone()).unwrap().eval().unwrap();
            }
        }
        Ok(Atom { data: CrispType::List(List::new()), symbol: sym::Symbol::List})
    }
}

// CrispFn trait with call(&self, List) method
// New struct for each individual external function
// Implements call() method to wire the args into the inputs.

mod parser {
    use crate::List;
    use crate::Atom;
    use crate::CrispType;
    use crate::sym;
    use crate::SExpr;
    use std::iter::Peekable;
    use std::str::Chars;
    use std::collections::HashMap;
    pub fn parse(string: &str) {
        let list = list(&mut string.chars().peekable());
        if let CrispType::List(list) = contextualize(list.into(), &crate::get_map()).data {
            println!("--- Evaluating...\n");
            SExpr::from_list(list).unwrap().eval().unwrap();
            println!("\n--- Evaluation completed.");
        }
    }
    
    pub fn contextualize(a: Atom, map: &HashMap<String, Atom>) -> Atom {
        match &a.data {
            CrispType::Symbol(s) => {
                if let sym::Symbol::Unknown(_) = &a.symbol {
                    if let Some(m) = map.get(s) {
                        m.clone()
                    } else {
                        a
                    }
                } else {
                    a
                }
            },
            CrispType::List(l) => {
                let lost: List = l.clone();
                lost.iter().map(|q| contextualize(q.clone(), &map)).collect::<List>().into()
            },
            _ => a,
        }
    }

    pub fn list(mut input: &mut Peekable<Chars<'_>>) -> List {
        let mut l = List::new();
        loop {
            match input.next() {
                Some(' ') => continue,
                Some('(') => {
                    l.push(Atom { data: CrispType::List(list(&mut input)), symbol: sym::Symbol::List })
                },
                Some(')') => {
                    return l
                },
                Some('"') => {
                    let funny = string(&mut input);
                    l.push(Atom { data: CrispType::Symbol(funny), symbol: crate::sym_string() });
                },
                Some(c) => {
                    let funny = c.to_string() + &mundane(&mut input);
                    l.push(Atom { data: CrispType::Symbol(funny), symbol: sym::Symbol::Unknown("Mundane".to_string()) });
                },
                None => return l,
            }
        }
    }

    fn mundane(mut input: &mut Peekable<Chars<'_>>) -> String {
        match input.peek() {
            Some(' ') => return "".to_string(), 
            Some(')') => return "".to_string(), 
            Some(_) => input.next().unwrap().to_string() + &mundane(&mut input),
            None => return "".to_string(),
        }
    }

    fn string(mut input: &mut Peekable<Chars<'_>>) -> String {
        match input.peek() {
            Some('"') => return String::new(),
            Some(')') => return String::new(),
            Some(_) => input.next().unwrap().to_string() + &string(&mut input),
            None => return String::new(),
        }
    }
}

fn sym_string() -> sym::Symbol {
    sym::Symbol::Struct(sym::Struct {
        name: "String".to_string(),
        traits: vec![
            "Add".to_string(),
            "Display".to_string(),
        ],
    })
}

fn get_map() -> std::collections::HashMap<String, Atom> {
    let mut map: std::collections::HashMap<String, Atom> = std::collections::HashMap::new();
    map.insert("add".to_string(), crate::CrispFunctionAdd::new().atomize());
    map.insert("println".to_string(), crate::CrispFunctionPrintln::new().atomize());
    map.insert("eval".to_string(), crate::CrispFunctionEval::new().atomize());
    map
}


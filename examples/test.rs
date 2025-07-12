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
    foo.push((21 as i32).convert());
    foo.push((21 as i32).convert());
    let bar: Atom = crispy_add(&foo[0], &foo[1]).unwrap();
    if let CrispType::Number(x) = bar.vtype { println!("{}", x) };
}

trait Crispy {
    fn convert(&self) -> Atom;
}

use std::rc::Rc;
#[derive(Clone)]
struct Atom {
    // data: Rc<dyn Crispy>,
    vtype: CrispType,
}

type Number = Rc<i32>;
#[derive(Clone)]
enum CrispType {
    List(List),
    Fn(CrispFunction),
    Number(Number),
}

type List = Vec<Atom>;

impl Crispy for i32 {
    fn convert(&self) -> Atom {
        Atom { vtype: CrispType::Number(Rc::new(self.clone())) }
    }
}

fn crispy_add(x: &Atom, y: &Atom) -> Result<Atom, ()> {
    if let CrispType::Number(x) = &x.vtype {
        if let CrispType::Number(y) = &y.vtype {
            Ok(((*x.clone() + *y.clone()) as i32).convert())
        } else { return Err(()) }
    } else { return Err(()) }
}

struct SExpr {
    func: CrispFunction,
    args: List,
}

impl SExpr {
    fn eval() {
        
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

#[derive(Clone)]
struct CrispFunction {
    function: Rc<dyn Fn()>,
    signature: CrispSignature,
}

#[derive(Clone)]
struct CrispSignature {
    builder: bool,
    args: Rc<List>,
    output: Rc<CrispType>,
}

fn crispify_function(function: Rc<dyn Fn()>, arg_types: &List, output: &CrispType) -> CrispFunction {
    CrispFunction {
        function: function.clone(),
        signature: CrispSignature {
            builder: false,
            args: Rc::new(arg_types.clone().to_vec()),
            output: Rc::new(output.clone()),
        }
    }
}

fn get_crisp_function() -> CrispFunction {
    let alist: &Vec<Atom> = &Vec::new();
    alist.push(0.convert());
    alist.push(0.convert());
    crispify_function(Rc::new(crispy_add), alist, &CrispType::Number(0.into()))
}

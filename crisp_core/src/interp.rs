use std::collections::HashMap;

use crate::CrispType;

pub struct GlobalContext {
    pub variables: HashMap<String, CrispType>
}

pub struct Interpreter {
    global_context: GlobalContext,
}

impl Interpreter  {
    pub fn new() -> Self {
        Self {
            global_context: GlobalContext { variables: HashMap::new() }
        }
    }
}

pub trait Evaluatable {
    fn eval(&self, interp: &mut Interpreter) -> CrispType;
}
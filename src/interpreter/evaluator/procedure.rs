use crate::interpreter::analysis::ast::{Parameter, ProcedureStatement};
use crate::interpreter::evaluator::errors::RuntimeResult;
use crate::interpreter::evaluator::object::Object;
use std::fmt;
use std::rc::Rc;

pub type BuiltinFn = Rc<Fn(&[Object]) -> RuntimeResult<Object>>;

#[derive(Clone)]
pub struct BuiltinProcedure {
    pub parameters: Vec<Parameter>,
    pub proc_fn: BuiltinFn,
}

impl fmt::Debug for BuiltinProcedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BuiltinProcedure")
            .field("parameters", &self.parameters)
            .field(
                "proc_fn",
                &String::from("Fn(&[Option] -> RuntimeResult<Object>)"),
            )
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum Procedure {
    UserDefined(ProcedureStatement),
    Builtin(BuiltinProcedure),
}

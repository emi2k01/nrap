use std::collections::HashMap;
use std::rc::Rc;

use crate::interpreter::evaluator::errors::{RuntimeError, RuntimeResult};
use crate::interpreter::evaluator::object::Object;
use crate::interpreter::evaluator::procedure::Procedure;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum EnvIdent<'a> {
    Borrowed(&'a str),
    Owned(String),
}

pub struct Environment<'a> {
    stores: Vec<Vec<HashMap<EnvIdent<'a>, Object>>>,
    procedures: HashMap<&'a str, Rc<Procedure>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            stores: vec![vec![HashMap::new()]],
            procedures: HashMap::new(),
        }
    }

    pub fn new_builtin() -> Self {
        let mut env = Self::new();
        env.procedures = super::builtin::new_builtin_procedures();
        env
    }

    pub fn enter_scope(&mut self) {
        self.stores.last_mut().unwrap().push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.stores.last_mut().unwrap().pop();
    }

    pub fn enter_call_stack(&mut self) {
        self.stores.push(vec![HashMap::new()]);
    }

    pub fn exit_call_stack(&mut self) {
        self.stores.pop();
    }

    pub fn add_procedure(&mut self, key: &'a str, proc: Procedure) -> RuntimeResult<()> {
        if self.procedures.contains_key(&key) {
            Err(RuntimeError::Generic(String::from(
                "Tried to add a procedure that already exists",
            )))
        } else {
            self.procedures.insert(key, Rc::new(proc));
            Ok(())
        }
    }

    pub fn get_procedure(&self, key: &str) -> RuntimeResult<Rc<Procedure>> {
        if let Some(proc) = self.procedures.get(key) {
            Ok(Rc::clone(proc))
        } else {
            Err(RuntimeError::Generic(String::from(
                "Tried to get a procedure that doesn't exist",
            )))
        }
    }

    pub fn set(&mut self, key: EnvIdent<'a>, obj: Object) {
        let rev_scopes = self.stores.last_mut().unwrap().iter_mut().rev();
        for scope in rev_scopes {
            if scope.get(&key).is_some() {
                scope.insert(key, obj);
                return;
            }
        }
        self.current_scope().insert(key, obj);
    }

    pub fn get(&mut self, obj: &EnvIdent<'a>) -> RuntimeResult<Object> {
        let rev_scopes = self.stores.last().unwrap().iter().rev();
        for scope in rev_scopes {
            if let Some(v) = scope.get(obj) {
                return Ok(v.to_owned());
            }
        }
        Err(RuntimeError::Generic(String::from(
            "Tried to get a variable that doesn't exist",
        )))
    }

    fn current_scope(&mut self) -> &mut HashMap<EnvIdent<'a>, Object> {
        self.stores.last_mut().unwrap().last_mut().unwrap()
    }
}

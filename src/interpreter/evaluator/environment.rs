use std::collections::HashMap;

use crate::interpreter::analysis::ast::ProcedureStatement;
use crate::interpreter::evaluator::errors::{RuntimeError, RuntimeResult};
use crate::interpreter::evaluator::object::Object;
use crate::interpreter::evaluator::procedure::Procedure;
use core::borrow::BorrowMut;

pub struct Environment {
    stores: Vec<Vec<HashMap<String, Object>>>,
    procedures: HashMap<String, Procedure>,
}

impl Environment {
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

    pub fn add_procedure(&mut self, key: String, proc: Procedure) -> RuntimeResult<()> {
        if self.procedures.contains_key(&key) {
            Err(RuntimeError::Generic(String::from(
                "Tried to add a procedure that already exists",
            )))
        } else {
            self.procedures.insert(key, proc);
            Ok(())
        }
    }

    pub fn get_procedure(&self, key: &str) -> RuntimeResult<&Procedure> {
        if let Some(proc) = self.procedures.get(key) {
            Ok(&proc)
        } else {
            Err(RuntimeError::Generic(String::from(
                "Tried to get a procedure that doesn't exist",
            )))
        }
    }

    pub fn set(&mut self, key: String, obj: Object) {
        let mut rev_scopes = self.stores.last_mut().unwrap().iter_mut().rev();
        for scope in rev_scopes {
            if let Some(v) = scope.get(&key) {
                scope.insert(key, obj);
                return;
            }
        }
        self.current_scope().insert(key, obj);
    }

    pub fn get(&mut self, obj: &str) -> RuntimeResult<Object> {
        let mut rev_scopes = self.stores.last().unwrap().iter().rev();
        for scope in rev_scopes {
            if let Some(v) = scope.get(obj) {
                return Ok(v.to_owned());
            }
        }
        Err(RuntimeError::Generic(String::from(
            "Tried to get a variable that doesn't exist",
        )))
    }

    fn current_scope(&mut self) -> &mut HashMap<String, Object> {
        self.stores.last_mut().unwrap().last_mut().unwrap()
    }
}

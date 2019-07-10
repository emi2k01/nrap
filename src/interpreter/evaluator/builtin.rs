use crate::interpreter::analysis::ast::Expression::Ident;
use crate::interpreter::analysis::ast::Parameter;
use crate::interpreter::evaluator::errors::{RuntimeError, RuntimeResult};
use crate::interpreter::evaluator::object::Object;
use crate::interpreter::evaluator::procedure::{BuiltinProcedure, Procedure};
use std::collections::HashMap;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;

pub fn new_builtin_procedures() -> HashMap<String, Procedure> {
    let mut procs = HashMap::new();

    let output_proc = BuiltinProcedure {
        parameters: vec![
            Parameter::new(String::from("output"), true, false),
            Parameter::new(String::from("break_line"), true, false),
        ],
        proc_fn: Rc::new(builtin_output),
    };

    let input_proc = BuiltinProcedure {
        parameters: vec![
            Parameter::new(String::from("prompt"), true, false),
            Parameter::new(String::from("input"), false, true),
        ],
        proc_fn: Rc::new(builtin_input),
    };

    procs.insert(String::from("output"), Procedure::Builtin(output_proc));
    procs.insert(String::from("input"), Procedure::Builtin(input_proc));
    procs
}

fn builtin_output(args: &[Object]) -> RuntimeResult<Object> {
    let output = &args[0];
    let break_line = &args[1];

    match break_line {
        Object::Bool(v) if *v => {
            println!("{}", output);
        }
        Object::Bool(_) => print!("{}", output),
        _ => {
            return Err(RuntimeError::Generic(String::from(
                "Expected a bool on output(output, break_line) but got another type",
            )))
        }
    }
    stdout().flush().unwrap();

    Ok(Object::None)
}

fn builtin_input(args: &[Object]) -> RuntimeResult<Object> {
    let mut buf = String::new();
    print!("{}", args[0]);
    stdout().flush().unwrap();
    stdin().read_line(&mut buf).unwrap();
    if let Some('\n') = buf.chars().next_back() {
        buf.pop();
    }
    if let Some('\r') = buf.chars().next_back() {
        buf.pop();
    }
    if let Ok(number) = buf.parse::<f64>() {
        return Ok(Object::Float(number));
    }
    Ok(Object::String(buf))
}

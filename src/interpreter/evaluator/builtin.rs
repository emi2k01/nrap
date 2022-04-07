use crate::interpreter::analysis::ast::Parameter;
use crate::interpreter::evaluator::errors::{RuntimeError, RuntimeResult};
use crate::interpreter::evaluator::object::Object;
use crate::interpreter::evaluator::procedure::{BuiltinProcedure, Procedure};
use std::collections::HashMap;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;
use wasm_bindgen::prelude::*;

pub fn new_builtin_procedures() -> HashMap<&'static str, Procedure> {
    let mut procs = HashMap::new();

    let output_proc = BuiltinProcedure {
        parameters: vec![
            Parameter::new(String::from("salida"), true, false),
            Parameter::new(String::from("salto_linea"), true, false),
        ],
        proc_fn: Rc::new(builtin_output),
    };

    let input_proc = BuiltinProcedure {
        parameters: vec![
            Parameter::new(String::from("mensaje"), true, false),
            Parameter::new(String::from("entrada"), false, true),
        ],
        proc_fn: Rc::new(builtin_input),
    };

    procs.insert("imprimir", Procedure::Builtin(output_proc));
    //procs.insert("leer", Procedure::Builtin(input_proc));
    procs
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = Out)]
    fn log(s: &str, newline: bool);
}

fn builtin_output(args: &[Object]) -> RuntimeResult<Object> {
    #[cfg(target_arch = "wasm32")]
    fn print<T: std::fmt::Display>(v: T, newline: bool) {
        log(&v.to_string(), newline);
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn print<T: Display>(v: &T, newline: bool) {
        if newline {
            println!("{}", &v.to_string());
        } else {
            print!("{}", &v.to_string());
        }
        stdout().flush().unwrap();
    }

    let output = &args[0];
    let break_line = &args[1];

    match break_line {
        Object::Bool(v) => {
            print(output, *v);
        }
        _ => {
            return Err(RuntimeError::Generic(String::from(
                "Expected a bool on imprimir(salida, salto_linea) but got another type",
            )))
        }
    }

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

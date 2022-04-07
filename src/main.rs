mod interpreter;

use std::fs;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{HtmlElement, HtmlInputElement};

use crate::interpreter::analysis::lexer::Lexer;
use crate::interpreter::analysis::parser::Parser;
use crate::interpreter::evaluator::{environment::Environment, Evaluator};

#[cfg(not(target_arch = "wasm32"))]
fn main() {
    let file_path = std::env::args().nth(1).unwrap();
    let program_content = fs::read_to_string(file_path).unwrap();
    let program_ast = Parser::new(Lexer::new(program_content.as_str()))
        .parse()
        .unwrap();
    let procedures = Evaluator::get_procedures(&program_ast, true).unwrap();
    Evaluator::new(Environment::new(), &program_ast, &procedures)
        .unwrap()
        .eval()
        .unwrap();
}

#[cfg(target_arch = "wasm32")]
fn main() {
    console_error_panic_hook::set_once();
    let run_callback = Closure::wrap(Box::new(move || {
        let document = web_sys::window().unwrap().document().unwrap();
        let outputEl = document
            .get_element_by_id("outputBox")
            .unwrap()
            .unchecked_into::<HtmlElement>()
            .set_inner_text("");
        let inputEl = document
            .get_element_by_id("inputBox")
            .unwrap()
            .unchecked_into::<HtmlInputElement>();
        let code = inputEl.value();
        run(code);
    }) as Box<dyn FnMut()>)
    .into_js_value();
    let document = web_sys::window().unwrap().document().unwrap();
    document
        .get_element_by_id("run")
        .unwrap()
        .add_event_listener_with_callback("click", run_callback.unchecked_ref());
}

fn run(program_content: String) {
    let program_ast = Parser::new(Lexer::new(&program_content)).parse().unwrap();
    let procedures = Evaluator::get_procedures(&program_ast, true).unwrap();
    Evaluator::new(Environment::new(), &program_ast, &procedures)
        .unwrap()
        .eval()
        .unwrap();
}

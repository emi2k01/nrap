mod interpreter;

use std::fs;
use std::io::{stdin, stdout};

use crate::interpreter::analysis::lexer::Lexer;
use crate::interpreter::analysis::parser::Parser;
use crate::interpreter::evaluator::{environment::Environment, Evaluator};

fn main() {
    let file_path = std::env::args().nth(1).unwrap();
    let program_content = fs::read_to_string(file_path).unwrap();
    let program_ast = Parser::new(Lexer::new(program_content.as_str()))
        .parse()
        .unwrap();
    let evaluator = Evaluator::new(Environment::new_builtin())
        .eval(program_ast)
        .unwrap();
}

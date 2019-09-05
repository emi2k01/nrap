mod interpreter;

use std::fs;

use crate::interpreter::analysis::lexer::Lexer;
use crate::interpreter::analysis::parser::Parser;
use crate::interpreter::evaluator::{environment::Environment, Evaluator};

fn main() {
    let file_path = std::env::args().nth(1).unwrap();
    let program_content = fs::read_to_string(file_path).unwrap();
    let program_ast = Parser::new(Lexer::new(program_content.as_str()))
        .parse()
        .unwrap();
    let procedures = Evaluator::get_procedures(&program_ast, true).unwrap();
    Evaluator::new(Environment::new_builtin(), &program_ast, &procedures)
        .unwrap()
        .eval()
        .unwrap();
}

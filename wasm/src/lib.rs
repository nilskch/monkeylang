use compiler::{
    evaluator::Evaluator, lexer::Lexer, object::environment::Environment, parser::Parser,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn eval_monkey_code(input: String) -> String {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => return err.to_string(),
    };

    let env = Environment::new();
    let mut evaluator = Evaluator::new();
    if let Err(err) = evaluator.eval_program(program, &env) {
        return err.to_string();
    }

    evaluator.output_buffer
}

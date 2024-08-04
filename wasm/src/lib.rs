use compiler::{evaluator::Evaluator, parser::Parser};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn eval_monkey_code(input: String) -> String {
    let mut parser = Parser::new(input.to_string());
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => return err.to_string(),
    };
    let mut evaluator = Evaluator::new();
    if let Err(err) = evaluator.eval_program(program) {
        return err.to_string();
    }

    evaluator.output_buffer
}

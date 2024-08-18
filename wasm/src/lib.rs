use compiler::{evaluator::Evaluator, parser::Parser};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn eval_monkey_code(input: String) -> Result<String, String> {
    let mut parser = Parser::new(input.to_string());
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => return Err(err.to_string()),
    };
    let mut evaluator = Evaluator::default();
    if let Err(err) = evaluator.eval_program(program) {
        return Err(err.to_string());
    }

    Ok(evaluator.output_buffer)
}

#[wasm_bindgen]
pub fn format_monkey_code(input: String) -> Result<String, String> {
    let mut parser = Parser::new(input.to_string());
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => return Err(err.to_string()),
    };
    Ok(program.format())
}

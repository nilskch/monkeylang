use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn eval_monkey_code(input: &str) -> String {
    String::from(input)
}

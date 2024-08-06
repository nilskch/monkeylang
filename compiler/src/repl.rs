use crate::evaluator::Evaluator;
use crate::object::Object;
use crate::parser::Parser;
use std::io;
use std::io::Write;

const PROMPT: &str = ">> ";

pub fn start() {
    println!("Welcome to the Monkey Programming Language!");
    let mut evaluator = Evaluator::new();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let mut parser = Parser::new(input.to_string());

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(err) => {
                println!("{err}");
                continue;
            }
        };

        let result = match evaluator.eval_program(program) {
            Ok(result) => result,
            Err(err) => {
                println!("{err}");
                continue;
            }
        };

        print!("{}", evaluator.output_buffer);
        evaluator.reset_output_buffer();

        match result {
            Object::Null => continue,
            _ => println!("{}", result),
        }
    }
}

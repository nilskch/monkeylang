use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::object::environment::Environment;
use crate::object::Object;
use crate::parser::Parser;
use std::io;
use std::io::Write;

const PROMPT: &str = ">> ";

pub fn start() {
    println!("Welcome to the Monkey Programming Language!");
    let env = Environment::new();
    let mut evaluator = Evaluator::new();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(err) => {
                println!("{err}");
                continue;
            }
        };

        let result = match evaluator.eval_program(program, &env) {
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

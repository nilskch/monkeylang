use crate::object::environment::Environment;

use super::evaluator::eval_program;
use super::lexer::Lexer;
use super::object::Object;
use super::parser::Parser;
use std::io;
use std::io::Write;

const PROMPT: &str = ">> ";

pub fn start() {
    let mut env = Environment::new();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if parser.errors.len() != 0 {
            print_parser_errors(parser.errors);
            continue;
        }

        let evaluated = eval_program(program, &mut env);
        match evaluated {
            Object::Null => continue,
            _ => println!("{}", evaluated.inspect()),
        }
    }
}

fn print_parser_errors(errors: Vec<String>) {
    for error in errors {
        println!("{}", error);
    }
}

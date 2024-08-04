use compiler::evaluator::Evaluator;
use compiler::lexer::Lexer;
use compiler::object::environment::Environment;
use compiler::parser::Parser;
use std::fs;
use std::process;

pub fn run(file_name: &String) {
    let source_code = match fs::read_to_string(file_name) {
        Ok(source_code) => source_code,
        Err(_) => {
            println!("ERROR: Can't open file '{file_name}'.");
            process::exit(1);
        }
    };

    let lexer = Lexer::new(source_code);
    let mut parser = Parser::new(lexer);
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => {
            println!("{err}");
            process::exit(1);
        }
    };

    let env = Environment::new();
    let mut evaluator = Evaluator::new();
    if let Err(err) = evaluator.eval_program(program, &env) {
        println!("{err}");
        process::exit(1);
    }
}

use compiler::evaluator::Evaluator;
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

    let mut parser = Parser::new(source_code);
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => {
            println!("{err}");
            process::exit(1);
        }
    };

    let mut evaluator = Evaluator::default();
    if let Err(err) = evaluator.eval_program(program) {
        println!("{err}");
        process::exit(1);
    }

    // print output_buffer to console
    println!("{}", evaluator.output_buffer);
}

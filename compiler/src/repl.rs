use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::object::environment::Environment;
use crate::object::Object;
use crate::parser::Parser;
use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;

const PROMPT: &str = ">> ";

pub fn start() {
    let env = &Rc::from(RefCell::new(Environment::new()));

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
                println!("{}", err);
                continue;
            }
        };

        match eval_program(program, env) {
            Ok(result) => match result {
                Object::Null => continue,
                _ => println!("{}", result.inspect()),
            },
            Err(err) => println!("ERROR: {}", err),
        }
    }
}

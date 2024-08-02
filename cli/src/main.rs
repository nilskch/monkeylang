use clap::{Arg, ArgAction, Command};
use compiler::{
    evaluator::eval_program, lexer::Lexer, object::environment::Environment, parser::Parser,
};
use std::{cell::RefCell, fs, rc::Rc};

fn main() {
    let matches = Command::new("monkey")
        .author("Nils Koch, mail@nilskch.dev")
        .version("1.0.2")
        .about("Use the monkey cli to run or format monkey code!")
        .arg(
            Arg::new("file")
                .help("Path to the .mky file.")
                .action(ArgAction::Set),
        )
        .get_matches();

    if !matches.args_present() {
        println!("Welcome to the Monkey Programming Language!");
        return compiler::repl::start();
    }

    let file_path = matches.get_one::<String>("file").unwrap();
    let monkey_code = fs::read_to_string(file_path).expect("Unable to read file");
    let lexer = Lexer::new(monkey_code);
    let mut parser = Parser::new(lexer);
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(err) => return println!("{err}"),
    };

    let env = &Rc::from(RefCell::new(Environment::new()));
    if let Err(err) = eval_program(program, env) {
        println!("{err}");
    }
}

use super::lexer::Lexer;
use super::parser::Parser;
use std::io;
use std::io::Write;

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let _program = parser.parse_program();
    }
}

use super::lexer::Lexer;
use super::token::TokenType;
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

        let mut lexer = Lexer::new(input);
        let mut token = lexer.next_token();

        loop {
            match token.token_type {
                TokenType::Eof => break,
                _ => {
                    println!("{:?}", token);
                    token = lexer.next_token();
                }
            }
        }
    }
}

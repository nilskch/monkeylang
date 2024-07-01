use crate::lexer::Lexer;
use crate::token::Token;

struct Parser {
    l: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Parser {
        Parser {
            cur_token: l.next_token(),
            peek_token: l.next_token(),
            l,
        }
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        let lexer = Lexer::new(String::from("foobar"));
        let mut parser = Parser::new(lexer);

        parser.next_token()
        // TODO: write tests
    }
}

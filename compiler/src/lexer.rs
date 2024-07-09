use super::token::{lookup_ident, Token, TokenType};

pub struct Lexer {
    input: String,
    number_of_chars: usize, // used support UTF-8 encoding (String.len() != number of characters in the string)
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let number_of_chars = input.chars().count();
        let mut l = Lexer {
            input,
            number_of_chars,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Eq, "==".into())
                } else {
                    Token::new(TokenType::Assign, self.ch.into())
                }
            }
            '+' => Token::new(TokenType::Plus, self.ch.into()),
            '-' => Token::new(TokenType::Minus, self.ch.into()),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".into())
                } else {
                    Token::new(TokenType::Bang, self.ch.into())
                }
            }
            '/' => Token::new(TokenType::Slash, self.ch.into()),
            '*' => Token::new(TokenType::Asterik, self.ch.into()),
            '<' => Token::new(TokenType::Lt, self.ch.into()),
            '>' => Token::new(TokenType::Gt, self.ch.into()),
            ';' => Token::new(TokenType::Semicolon, self.ch.into()),
            ',' => Token::new(TokenType::Comma, self.ch.into()),
            '{' => Token::new(TokenType::LBrace, self.ch.into()),
            '}' => Token::new(TokenType::RBrace, self.ch.into()),
            '(' => Token::new(TokenType::LParen, self.ch.into()),
            ')' => Token::new(TokenType::RParen, self.ch.into()),
            '\0' => Token::new(TokenType::Eof, "".into()),
            _ => {
                if self.is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token_type = lookup_ident(&literal);
                    // next line is an early return for the function, not variable assignment
                    return Token::new(token_type, literal);
                }
                if self.is_digit(self.ch) {
                    let literal = self.read_number();
                    // next line is an early return for the function, not variable assignment
                    return Token::new(TokenType::Int, literal);
                }
                Token::new(TokenType::Illegal, self.ch.into())
            }
        };

        self.read_char();
        token
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len().into() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.number_of_chars {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].into()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.is_digit(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].into()
    }

    // using this function instead of the std lib functions to get exact control
    fn is_digit(&mut self, ch: char) -> bool {
        '0' <= ch && ch <= '9'
    }

    // using this function instead of the std lib functions to get exact control
    fn is_letter(&mut self, ch: char) -> bool {
        'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        // this is the exact same test as in the book
        let input = "
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
                x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;";

        let tests = [
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterik, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::Gt, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::Eof, ""),
        ];

        let mut lexer = Lexer::new(input.into());
        for (expected_type, expected_literal) in tests.iter() {
            let token = lexer.next_token();
            assert_eq!(token.token_type, *expected_type);
            assert_eq!(token.literal, *expected_literal);
        }
    }
}

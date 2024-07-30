use super::token::{lookup_ident, Token, TokenType};

pub struct Lexer {
    input: String,
    number_of_chars: usize, // used support UTF-8 encoding (String.len() != number of characters in the string)
    position: usize,
    read_position: usize,
    ch: char,
    line: i64,
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
            line: 1,
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
                    Token::new(TokenType::Eq, "==".into(), self.line)
                } else {
                    Token::new(TokenType::Assign, self.ch.into(), self.line)
                }
            }
            '+' => Token::new(TokenType::Plus, self.ch.into(), self.line),
            '-' => Token::new(TokenType::Minus, self.ch.into(), self.line),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".into(), self.line)
                } else {
                    Token::new(TokenType::Bang, self.ch.into(), self.line)
                }
            }
            '/' => Token::new(TokenType::Slash, self.ch.into(), self.line),
            '*' => Token::new(TokenType::Asterik, self.ch.into(), self.line),
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::LtEq, "<=".into(), self.line)
                } else {
                    Token::new(TokenType::Lt, self.ch.into(), self.line)
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::GtEq, ">=".into(), self.line)
                } else {
                    Token::new(TokenType::Gt, self.ch.into(), self.line)
                }
            }
            ';' => Token::new(TokenType::Semicolon, self.ch.into(), self.line),
            ':' => Token::new(TokenType::Colon, self.ch.into(), self.line),
            ',' => Token::new(TokenType::Comma, self.ch.into(), self.line),
            '{' => Token::new(TokenType::LBrace, self.ch.into(), self.line),
            '}' => Token::new(TokenType::RBrace, self.ch.into(), self.line),
            '[' => Token::new(TokenType::LBracket, self.ch.into(), self.line),
            ']' => Token::new(TokenType::RBracket, self.ch.into(), self.line),
            '(' => Token::new(TokenType::LParen, self.ch.into(), self.line),
            ')' => Token::new(TokenType::RParen, self.ch.into(), self.line),
            '\0' => Token::new(TokenType::Eof, "".into(), self.line),
            '"' => Token::new(TokenType::String, self.read_string(), self.line),
            _ => {
                if self.is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token_type = lookup_ident(&literal);
                    // next line is an early return for the function, not variable assignment
                    return Token::new(token_type, literal, self.line);
                }
                if self.is_digit(self.ch) {
                    let literal = self.read_number();
                    // next line is an early return for the function, not variable assignment
                    return Token::new(TokenType::Int, literal, self.line);
                }
                Token::new(TokenType::Illegal, self.ch.into(), self.line)
            }
        };

        self.read_char();
        token
    }

    fn read_string(&mut self) -> String {
        let pos = self.position + 1;
        self.read_char();

        while self.ch != '"' && self.ch != '\0' {
            self.read_char();
        }
        return self.input[pos..self.position].into();
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
            if self.ch == '\n' {
                self.line += 1;
            }
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
        10 != 9;
        10 <= 10;
        10 >= 10;
        \"foobar\"
        \"foo bar\"
        [1, 2];
        {\"foo\": \"bar\"}";

        let tests = [
            Token::new(TokenType::Let, "let".into(), 2),
            Token::new(TokenType::Ident, "five".into(), 2),
            Token::new(TokenType::Assign, "=".into(), 2),
            Token::new(TokenType::Int, "5".into(), 2),
            Token::new(TokenType::Semicolon, ";".into(), 2),
            Token::new(TokenType::Let, "let".into(), 3),
            Token::new(TokenType::Ident, "ten".into(), 3),
            Token::new(TokenType::Assign, "=".into(), 3),
            Token::new(TokenType::Int, "10".into(), 3),
            Token::new(TokenType::Semicolon, ";".into(), 3),
            Token::new(TokenType::Let, "let".into(), 5),
            Token::new(TokenType::Ident, "add".into(), 5),
            Token::new(TokenType::Assign, "=".into(), 5),
            Token::new(TokenType::Function, "fn".into(), 5),
            Token::new(TokenType::LParen, "(".into(), 5),
            Token::new(TokenType::Ident, "x".into(), 5),
            Token::new(TokenType::Comma, ",".into(), 5),
            Token::new(TokenType::Ident, "y".into(), 5),
            Token::new(TokenType::RParen, ")".into(), 5),
            Token::new(TokenType::LBrace, "{".into(), 5),
            Token::new(TokenType::Ident, "x".into(), 6),
            Token::new(TokenType::Plus, "+".into(), 6),
            Token::new(TokenType::Ident, "y".into(), 6),
            Token::new(TokenType::Semicolon, ";".into(), 6),
            Token::new(TokenType::RBrace, "}".into(), 7),
            Token::new(TokenType::Semicolon, ";".into(), 7),
            Token::new(TokenType::Let, "let".into(), 9),
            Token::new(TokenType::Ident, "result".into(), 9),
            Token::new(TokenType::Assign, "=".into(), 9),
            Token::new(TokenType::Ident, "add".into(), 9),
            Token::new(TokenType::LParen, "(".into(), 9),
            Token::new(TokenType::Ident, "five".into(), 9),
            Token::new(TokenType::Comma, ",".into(), 9),
            Token::new(TokenType::Ident, "ten".into(), 9),
            Token::new(TokenType::RParen, ")".into(), 9),
            Token::new(TokenType::Semicolon, ";".into(), 9),
            Token::new(TokenType::Bang, "!".into(), 10),
            Token::new(TokenType::Minus, "-".into(), 10),
            Token::new(TokenType::Slash, "/".into(), 10),
            Token::new(TokenType::Asterik, "*".into(), 10),
            Token::new(TokenType::Int, "5".into(), 10),
            Token::new(TokenType::Semicolon, ";".into(), 10),
            Token::new(TokenType::Int, "5".into(), 11),
            Token::new(TokenType::Lt, "<".into(), 11),
            Token::new(TokenType::Int, "10".into(), 11),
            Token::new(TokenType::Gt, ">".into(), 11),
            Token::new(TokenType::Int, "5".into(), 11),
            Token::new(TokenType::Semicolon, ";".into(), 11),
            Token::new(TokenType::If, "if".into(), 13),
            Token::new(TokenType::LParen, "(".into(), 13),
            Token::new(TokenType::Int, "5".into(), 13),
            Token::new(TokenType::Lt, "<".into(), 13),
            Token::new(TokenType::Int, "10".into(), 13),
            Token::new(TokenType::RParen, ")".into(), 13),
            Token::new(TokenType::LBrace, "{".into(), 13),
            Token::new(TokenType::Return, "return".into(), 14),
            Token::new(TokenType::True, "true".into(), 14),
            Token::new(TokenType::Semicolon, ";".into(), 14),
            Token::new(TokenType::RBrace, "}".into(), 15),
            Token::new(TokenType::Else, "else".into(), 15),
            Token::new(TokenType::LBrace, "{".into(), 15),
            Token::new(TokenType::Return, "return".into(), 16),
            Token::new(TokenType::False, "false".into(), 16),
            Token::new(TokenType::Semicolon, ";".into(), 16),
            Token::new(TokenType::RBrace, "}".into(), 17),
            Token::new(TokenType::Int, "10".into(), 19),
            Token::new(TokenType::Eq, "==".into(), 19),
            Token::new(TokenType::Int, "10".into(), 19),
            Token::new(TokenType::Semicolon, ";".into(), 19),
            Token::new(TokenType::Int, "10".into(), 20),
            Token::new(TokenType::NotEq, "!=".into(), 20),
            Token::new(TokenType::Int, "9".into(), 20),
            Token::new(TokenType::Semicolon, ";".into(), 20),
            Token::new(TokenType::Int, "10".into(), 21),
            Token::new(TokenType::LtEq, "<=".into(), 21),
            Token::new(TokenType::Int, "10".into(), 21),
            Token::new(TokenType::Semicolon, ";".into(), 21),
            Token::new(TokenType::Int, "10".into(), 22),
            Token::new(TokenType::GtEq, ">=".into(), 22),
            Token::new(TokenType::Int, "10".into(), 22),
            Token::new(TokenType::Semicolon, ";".into(), 22),
            Token::new(TokenType::String, "foobar".into(), 23),
            Token::new(TokenType::String, "foo bar".into(), 24),
            Token::new(TokenType::LBracket, "[".into(), 25),
            Token::new(TokenType::Int, "1".into(), 25),
            Token::new(TokenType::Comma, ",".into(), 25),
            Token::new(TokenType::Int, "2".into(), 25),
            Token::new(TokenType::RBracket, "]".into(), 25),
            Token::new(TokenType::Semicolon, ";".into(), 25),
            Token::new(TokenType::LBrace, "{".into(), 26),
            Token::new(TokenType::String, "foo".into(), 26),
            Token::new(TokenType::Colon, ":".into(), 26),
            Token::new(TokenType::String, "bar".into(), 26),
            Token::new(TokenType::RBrace, "}".into(), 26),
            Token::new(TokenType::Eof, "".into(), 26),
        ];

        let mut lexer = Lexer::new(input.into());
        for expected_token in tests.iter() {
            let token = lexer.next_token();
            assert_eq!(token.token_type, expected_token.token_type);
            assert_eq!(token.literal, expected_token.literal);
            assert_eq!(token.line, expected_token.line);
        }
    }
}

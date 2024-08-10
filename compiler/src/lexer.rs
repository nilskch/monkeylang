use super::token::{lookup_ident, Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
    line: i64,
    col: i64,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
            line: 1,
            col: 0,
        };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token {
        if let Some(token) = self.skip_whitespace() {
            return token;
        }

        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Eq, "==".into(), (self.line, self.col - 1))
                } else {
                    Token::new(TokenType::Assign, self.ch.into(), (self.line, self.col))
                }
            }
            '+' => Token::new(TokenType::Plus, self.ch.into(), (self.line, self.col)),
            '-' => Token::new(TokenType::Minus, self.ch.into(), (self.line, self.col)),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".into(), (self.line, self.col - 1))
                } else {
                    Token::new(TokenType::Bang, self.ch.into(), (self.line, self.col))
                }
            }
            '/' => Token::new(TokenType::Slash, self.ch.into(), (self.line, self.col)),
            '&' => {
                let position = (self.line, self.col);
                if self.peek_char() == '&' {
                    self.read_char();
                    Token::new(TokenType::And, "&&".into(), position)
                } else {
                    Token::new(TokenType::Illegal, self.ch.into(), position)
                }
            }
            '|' => {
                let position = (self.line, self.col);
                if self.peek_char() == '|' {
                    self.read_char();
                    Token::new(TokenType::Or, "||".into(), position)
                } else {
                    Token::new(TokenType::Illegal, self.ch.into(), position)
                }
            }
            '*' => Token::new(TokenType::Asterik, self.ch.into(), (self.line, self.col)),
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::LtEq, "<=".into(), (self.line, self.col - 1))
                } else {
                    Token::new(TokenType::Lt, self.ch.into(), (self.line, self.col))
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::GtEq, ">=".into(), (self.line, self.col - 1))
                } else {
                    Token::new(TokenType::Gt, self.ch.into(), (self.line, self.col))
                }
            }
            ';' => Token::new(TokenType::Semicolon, self.ch.into(), (self.line, self.col)),
            ':' => Token::new(TokenType::Colon, self.ch.into(), (self.line, self.col)),
            ',' => Token::new(TokenType::Comma, self.ch.into(), (self.line, self.col)),
            '{' => Token::new(TokenType::LBrace, self.ch.into(), (self.line, self.col)),
            '}' => Token::new(TokenType::RBrace, self.ch.into(), (self.line, self.col)),
            '[' => Token::new(TokenType::LBracket, self.ch.into(), (self.line, self.col)),
            ']' => Token::new(TokenType::RBracket, self.ch.into(), (self.line, self.col)),
            '(' => Token::new(TokenType::LParen, self.ch.into(), (self.line, self.col)),
            ')' => Token::new(TokenType::RParen, self.ch.into(), (self.line, self.col)),
            '\0' => Token::new(TokenType::Eof, "".into(), (self.line, self.col)),
            '"' => Token::new(TokenType::String, self.read_string(), (self.line, self.col)),
            _ => {
                let position = (self.line, self.col);
                if self.is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token_type = lookup_ident(&literal);
                    // next line is an early return for the function, not variable assignment
                    return Token::new(token_type, literal, position);
                }
                if self.ch.is_ascii_digit() {
                    let literal = self.read_number();
                    // next line is an early return for the function, not variable assignment
                    return Token::new(TokenType::Int, literal, position);
                }
                Token::new(TokenType::Illegal, self.ch.into(), position)
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
        self.input[pos..self.position].into()
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    fn read_char(&mut self) {
        if self.ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1
        }
        self.ch = self.input.chars().nth(self.read_position).unwrap_or('\0');
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) -> Option<Token> {
        let mut line_break_count = 0;
        let mut line = 0;
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            if self.ch == '\n' {
                line_break_count += 1;
                if line_break_count == 1 {
                    line = self.line + 1;
                }
            }
            self.read_char();
        }
        if line_break_count >= 2 {
            Some(Token::new(TokenType::EmptyLine, "\n".into(), (line, 1)))
        } else {
            None
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
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        self.input[position..self.position].into()
    }

    // using this function instead of the std lib functions to get exact control
    fn is_letter(&self, ch: char) -> bool {
        ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        // this is the exact same test as in the book
        let input = "if(;)
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
{\"foo\": \"bar\"}



if(;)
&&
|| &&



x";

        let tests = [
            Token::new(TokenType::If, "if".into(), (1, 1)),
            Token::new(TokenType::LParen, "(".into(), (1, 3)),
            Token::new(TokenType::Semicolon, ";".into(), (1, 4)),
            Token::new(TokenType::RParen, ")".into(), (1, 5)),
            Token::new(TokenType::Let, "let".into(), (2, 1)),
            Token::new(TokenType::Ident, "five".into(), (2, 5)),
            Token::new(TokenType::Assign, "=".into(), (2, 10)),
            Token::new(TokenType::Int, "5".into(), (2, 12)),
            Token::new(TokenType::Semicolon, ";".into(), (2, 13)),
            Token::new(TokenType::Let, "let".into(), (3, 1)),
            Token::new(TokenType::Ident, "ten".into(), (3, 5)),
            Token::new(TokenType::Assign, "=".into(), (3, 9)),
            Token::new(TokenType::Int, "10".into(), (3, 11)),
            Token::new(TokenType::Semicolon, ";".into(), (3, 13)),
            Token::new(TokenType::EmptyLine, "\n".into(), (4, 1)),
            Token::new(TokenType::Let, "let".into(), (5, 1)),
            Token::new(TokenType::Ident, "add".into(), (5, 5)),
            Token::new(TokenType::Assign, "=".into(), (5, 9)),
            Token::new(TokenType::Function, "fn".into(), (5, 11)),
            Token::new(TokenType::LParen, "(".into(), (5, 13)),
            Token::new(TokenType::Ident, "x".into(), (5, 14)),
            Token::new(TokenType::Comma, ",".into(), (5, 15)),
            Token::new(TokenType::Ident, "y".into(), (5, 17)),
            Token::new(TokenType::RParen, ")".into(), (5, 18)),
            Token::new(TokenType::LBrace, "{".into(), (5, 20)),
            Token::new(TokenType::Ident, "x".into(), (6, 9)),
            Token::new(TokenType::Plus, "+".into(), (6, 11)),
            Token::new(TokenType::Ident, "y".into(), (6, 13)),
            Token::new(TokenType::Semicolon, ";".into(), (6, 14)),
            Token::new(TokenType::RBrace, "}".into(), (7, 1)),
            Token::new(TokenType::Semicolon, ";".into(), (7, 2)),
            Token::new(TokenType::EmptyLine, "\n".into(), (8, 1)),
            Token::new(TokenType::Let, "let".into(), (9, 1)),
            Token::new(TokenType::Ident, "result".into(), (9, 5)),
            Token::new(TokenType::Assign, "=".into(), (9, 12)),
            Token::new(TokenType::Ident, "add".into(), (9, 14)),
            Token::new(TokenType::LParen, "(".into(), (9, 17)),
            Token::new(TokenType::Ident, "five".into(), (9, 18)),
            Token::new(TokenType::Comma, ",".into(), (9, 22)),
            Token::new(TokenType::Ident, "ten".into(), (9, 24)),
            Token::new(TokenType::RParen, ")".into(), (9, 27)),
            Token::new(TokenType::Semicolon, ";".into(), (9, 28)),
            Token::new(TokenType::Bang, "!".into(), (10, 1)),
            Token::new(TokenType::Minus, "-".into(), (10, 2)),
            Token::new(TokenType::Slash, "/".into(), (10, 3)),
            Token::new(TokenType::Asterik, "*".into(), (10, 4)),
            Token::new(TokenType::Int, "5".into(), (10, 5)),
            Token::new(TokenType::Semicolon, ";".into(), (10, 6)),
            Token::new(TokenType::Int, "5".into(), (11, 1)),
            Token::new(TokenType::Lt, "<".into(), (11, 3)),
            Token::new(TokenType::Int, "10".into(), (11, 5)),
            Token::new(TokenType::Gt, ">".into(), (11, 8)),
            Token::new(TokenType::Int, "5".into(), (11, 10)),
            Token::new(TokenType::Semicolon, ";".into(), (11, 11)),
            Token::new(TokenType::EmptyLine, "\n".into(), (12, 1)),
            Token::new(TokenType::If, "if".into(), (13, 1)),
            Token::new(TokenType::LParen, "(".into(), (13, 4)),
            Token::new(TokenType::Int, "5".into(), (13, 5)),
            Token::new(TokenType::Lt, "<".into(), (13, 7)),
            Token::new(TokenType::Int, "10".into(), (13, 9)),
            Token::new(TokenType::RParen, ")".into(), (13, 11)),
            Token::new(TokenType::LBrace, "{".into(), (13, 13)),
            Token::new(TokenType::Return, "return".into(), (14, 5)),
            Token::new(TokenType::True, "true".into(), (14, 12)),
            Token::new(TokenType::Semicolon, ";".into(), (14, 16)),
            Token::new(TokenType::RBrace, "}".into(), (15, 1)),
            Token::new(TokenType::Else, "else".into(), (15, 3)),
            Token::new(TokenType::LBrace, "{".into(), (15, 8)),
            Token::new(TokenType::Return, "return".into(), (16, 5)),
            Token::new(TokenType::False, "false".into(), (16, 12)),
            Token::new(TokenType::Semicolon, ";".into(), (16, 17)),
            Token::new(TokenType::RBrace, "}".into(), (17, 1)),
            Token::new(TokenType::EmptyLine, "\n".into(), (18, 1)),
            Token::new(TokenType::Int, "10".into(), (19, 1)),
            Token::new(TokenType::Eq, "==".into(), (19, 4)),
            Token::new(TokenType::Int, "10".into(), (19, 7)),
            Token::new(TokenType::Semicolon, ";".into(), (19, 9)),
            Token::new(TokenType::Int, "10".into(), (20, 1)),
            Token::new(TokenType::NotEq, "!=".into(), (20, 4)),
            Token::new(TokenType::Int, "9".into(), (20, 7)),
            Token::new(TokenType::Semicolon, ";".into(), (20, 8)),
            Token::new(TokenType::Int, "10".into(), (21, 1)),
            Token::new(TokenType::LtEq, "<=".into(), (21, 4)),
            Token::new(TokenType::Int, "10".into(), (21, 7)),
            Token::new(TokenType::Semicolon, ";".into(), (21, 9)),
            Token::new(TokenType::Int, "10".into(), (22, 1)),
            Token::new(TokenType::GtEq, ">=".into(), (22, 4)),
            Token::new(TokenType::Int, "10".into(), (22, 7)),
            Token::new(TokenType::Semicolon, ";".into(), (22, 9)),
            Token::new(TokenType::String, "foobar".into(), (23, 8)),
            Token::new(TokenType::String, "foo bar".into(), (24, 9)),
            Token::new(TokenType::LBracket, "[".into(), (25, 1)),
            Token::new(TokenType::Int, "1".into(), (25, 2)),
            Token::new(TokenType::Comma, ",".into(), (25, 3)),
            Token::new(TokenType::Int, "2".into(), (25, 5)),
            Token::new(TokenType::RBracket, "]".into(), (25, 6)),
            Token::new(TokenType::Semicolon, ";".into(), (25, 7)),
            Token::new(TokenType::LBrace, "{".into(), (26, 1)),
            Token::new(TokenType::String, "foo".into(), (26, 6)),
            Token::new(TokenType::Colon, ":".into(), (26, 7)),
            Token::new(TokenType::String, "bar".into(), (26, 13)),
            Token::new(TokenType::RBrace, "}".into(), (26, 14)),
            Token::new(TokenType::EmptyLine, "\n".into(), (27, 1)),
            Token::new(TokenType::If, "if".into(), (30, 1)),
            Token::new(TokenType::LParen, "(".into(), (30, 3)),
            Token::new(TokenType::Semicolon, ";".into(), (30, 4)),
            Token::new(TokenType::RParen, ")".into(), (30, 5)),
            Token::new(TokenType::And, "&&".into(), (31, 1)),
            Token::new(TokenType::Or, "||".into(), (32, 1)),
            Token::new(TokenType::And, "&&".into(), (32, 4)),
            Token::new(TokenType::EmptyLine, "\n".into(), (33, 1)),
            Token::new(TokenType::Ident, "x".into(), (36, 1)),
            Token::new(TokenType::Eof, "".into(), (36, 2)),
        ];

        let mut lexer = Lexer::new(input.into());
        for expected_token in tests.iter() {
            let token = lexer.next_token();
            assert_eq!(token.token_type, expected_token.token_type);
            assert_eq!(token.literal, expected_token.literal);

            let (expected_line, expected_col) = expected_token.position;
            let (line, col) = token.position;
            assert_eq!(
                expected_line, line,
                "Expected token to start at line {}, but got line {}",
                expected_line, line
            );
            assert_eq!(
                expected_col, col,
                "Expected token to start at col {}, but got col {}",
                expected_col, col
            );
        }
    }
}

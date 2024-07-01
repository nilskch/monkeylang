#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers + literals
    Ident,
    Int,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterik,
    Slash,

    Lt,
    Gt,

    Eq,
    NotEq,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Fucntion,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String) -> Token {
        Token {
            token_type,
            literal,
        }
    }
}

pub fn lookup_ident(ident: &str) -> TokenType {
    match ident {
        "fn" => TokenType::Fucntion,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        _ => TokenType::Ident,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lookup_ident() {
        // these tests are not in the book but i added the, nevertheless
        let tests = vec![
            ("fn", TokenType::Fucntion),
            ("let", TokenType::Let),
            ("true", TokenType::True),
            ("false", TokenType::False),
            ("if", TokenType::If),
            ("else", TokenType::Else),
            ("return", TokenType::Return),
            ("foobar", TokenType::Ident),
        ];

        for (input, expected) in tests {
            assert_eq!(lookup_ident(input), expected);
        }
    }
}

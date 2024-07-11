use std::fmt::{Display, Formatter, Result};

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
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
    LtEq,
    GtEq,

    Eq,
    NotEq,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            TokenType::Illegal => write!(f, "Illegal"),
            TokenType::Eof => write!(f, "Eof"),

            // Identifiers + literals
            TokenType::Ident => write!(f, "Ident"),
            TokenType::Int => write!(f, "Int"),

            // Operators
            TokenType::Assign => write!(f, "Assign"),
            TokenType::Plus => write!(f, "Plus"),
            TokenType::Minus => write!(f, "Minus"),
            TokenType::Bang => write!(f, "Bang"),
            TokenType::Asterik => write!(f, "Asterik"),
            TokenType::Slash => write!(f, "Slash"),

            TokenType::Lt => write!(f, "Lt"),
            TokenType::Gt => write!(f, "Gt"),
            TokenType::LtEq => write!(f, "LtEq"),
            TokenType::GtEq => write!(f, "GtEq"),

            TokenType::Eq => write!(f, "Eq"),
            TokenType::NotEq => write!(f, "NotEq"),

            TokenType::Comma => write!(f, "Comma"),
            TokenType::Semicolon => write!(f, "Semicolon"),

            TokenType::LParen => write!(f, "LParen"),
            TokenType::RParen => write!(f, "RParen"),
            TokenType::LBrace => write!(f, "LBrace"),
            TokenType::RBrace => write!(f, "RBrace"),

            // Keywords
            TokenType::Function => write!(f, "Function"),
            TokenType::Let => write!(f, "Let"),
            TokenType::True => write!(f, "True"),
            TokenType::False => write!(f, "False"),
            TokenType::If => write!(f, "If"),
            TokenType::Else => write!(f, "Else"),
            TokenType::Return => write!(f, "Return"),
        }
    }
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
        "fn" => TokenType::Function,
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
            ("fn", TokenType::Function),
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

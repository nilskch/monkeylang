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
    Colon,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    String,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            TokenType::Illegal => write!(f, "Illegal"),
            TokenType::Eof => write!(f, "Eof"),

            // Identifiers + literals
            TokenType::Ident => write!(f, "Ident"),
            TokenType::Int => write!(f, "int"),
            TokenType::String => write!(f, "string"),

            // Operators
            TokenType::Assign => write!(f, "="),
            TokenType::Plus => write!(f, "+"),
            TokenType::Minus => write!(f, "-"),
            TokenType::Bang => write!(f, "!"),
            TokenType::Asterik => write!(f, "*"),
            TokenType::Slash => write!(f, "/"),

            TokenType::Lt => write!(f, "<"),
            TokenType::Gt => write!(f, ">"),
            TokenType::LtEq => write!(f, "<="),
            TokenType::GtEq => write!(f, ">="),

            TokenType::Eq => write!(f, "=="),
            TokenType::NotEq => write!(f, "!="),

            TokenType::Comma => write!(f, ","),
            TokenType::Colon => write!(f, ":"),
            TokenType::Semicolon => write!(f, ";"),

            TokenType::LParen => write!(f, "("),
            TokenType::RParen => write!(f, ")"),
            TokenType::LBrace => write!(f, "{{"),
            TokenType::RBrace => write!(f, "}}"),
            TokenType::LBracket => write!(f, "["),
            TokenType::RBracket => write!(f, "]"),

            // Keywords
            TokenType::Function => write!(f, "fn"),
            TokenType::Let => write!(f, "let"),
            TokenType::True => write!(f, "true"),
            TokenType::False => write!(f, "false"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::Return => write!(f, "return"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
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

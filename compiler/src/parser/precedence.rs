use crate::token::TokenType;
use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Equals,      // ==
    LessGreater, // > or < or <= or >=
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

lazy_static! {
    pub static ref PRECEDENCES: HashMap<TokenType, Precedence> = {
        let mut m = HashMap::new();
        m.insert(TokenType::Eq, Precedence::Equals);
        m.insert(TokenType::NotEq, Precedence::Equals);
        m.insert(TokenType::Lt, Precedence::LessGreater);
        m.insert(TokenType::Gt, Precedence::LessGreater);
        m.insert(TokenType::LtEq, Precedence::LessGreater);
        m.insert(TokenType::GtEq, Precedence::LessGreater);
        m.insert(TokenType::Plus, Precedence::Sum);
        m.insert(TokenType::Minus, Precedence::Sum);
        m.insert(TokenType::Slash, Precedence::Product);
        m.insert(TokenType::Asterik, Precedence::Product);
        m.insert(TokenType::LParen, Precedence::Call);
        m
    };
}

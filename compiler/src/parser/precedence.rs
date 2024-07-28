use crate::token::TokenType;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Equals,      // ==
    LessGreater, // > or < or <= or >=
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Object,      // myFunction(X)
    Index,       // array[index]
}

impl Precedence {
    pub fn get(token_type: &TokenType) -> Option<Precedence> {
        match token_type {
            TokenType::Eq => Some(Precedence::Equals),
            TokenType::NotEq => Some(Precedence::Equals),
            TokenType::Lt => Some(Precedence::LessGreater),
            TokenType::Gt => Some(Precedence::LessGreater),
            TokenType::LtEq => Some(Precedence::LessGreater),
            TokenType::GtEq => Some(Precedence::LessGreater),
            TokenType::Plus => Some(Precedence::Sum),
            TokenType::Minus => Some(Precedence::Sum),
            TokenType::Slash => Some(Precedence::Product),
            TokenType::Asterik => Some(Precedence::Product),
            TokenType::LParen => Some(Precedence::Object),
            TokenType::LBracket => Some(Precedence::Index),
            _ => None,
        }
    }
}

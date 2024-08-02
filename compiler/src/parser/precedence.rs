use crate::token::TokenType;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Equals, // ==
    AndOr,
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
            TokenType::And | TokenType::Or => Some(Precedence::AndOr),
            TokenType::Eq | TokenType::NotEq => Some(Precedence::Equals),
            TokenType::Lt | TokenType::Gt | TokenType::LtEq | TokenType::GtEq => {
                Some(Precedence::LessGreater)
            }
            TokenType::Plus | TokenType::Minus => Some(Precedence::Sum),
            TokenType::Slash | TokenType::Asterik => Some(Precedence::Product),
            TokenType::LParen => Some(Precedence::Object),
            TokenType::LBracket => Some(Precedence::Index),
            _ => None,
        }
    }
}

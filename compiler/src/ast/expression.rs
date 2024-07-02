use super::Node;
use crate::token::Token;
use std::fmt::{Display, Formatter, Result};

#[derive(Clone)]
pub enum Expression {
    Ident(Identifier),
    Nil,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self {
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::Nil => unreachable!(),
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Ident(ident) => ident.token_literal(),
            Expression::Nil => unreachable!(),
        }
    }
}

#[derive(Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.value)
    }
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Identifier {
        Identifier { token, value }
    }
}

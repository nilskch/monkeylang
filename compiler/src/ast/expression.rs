use super::Node;
use crate::token::Token;
use std::{
    fmt::{Display, Formatter, Result},
    iter::Empty,
};

pub enum Expression {
    Ident(Identifier),
    Empty, // only for development
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self {
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::Empty => unreachable!(),
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Ident(ident) => ident.token_literal(),
            Expression::Empty => unreachable!(),
        }
    }
}

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
        write!(f, "let = {} = {};", self.token.literal, self.value)
    }
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Identifier {
        Identifier { token, value }
    }
}

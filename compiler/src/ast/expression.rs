use super::Node;
use crate::token::Token;
use std::fmt::{Display, Formatter, Result};

#[derive(Clone)]
pub enum Expression {
    Ident(Identifier),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Nil,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self {
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::Integer(integer) => write!(f, "{}", integer),
            Expression::Prefix(prefix_expr) => write!(f, "{}", prefix_expr),
            Expression::Nil => unreachable!(),
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Ident(ident) => ident.token_literal(),
            Expression::Integer(integer) => integer.token_literal(),
            Expression::Prefix(prefix_expr) => prefix_expr.token_literal(),
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

#[derive(Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.token.literal)
    }
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> IntegerLiteral {
        IntegerLiteral { token, value }
    }
}

#[derive(Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Expression) -> PrefixExpression {
        PrefixExpression {
            token,
            operator,
            right: Box::new(right),
        }
    }
}

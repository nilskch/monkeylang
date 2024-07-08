use crate::{ast::Node, token::Token};
use std::fmt::{Display, Formatter, Result};

use super::expression::{Expression, Identifier};

#[derive(Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(ExpressionStatement),
    Block(BlockStatement),
    Nil,
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Statement::Let(stmt) => write!(f, "{}", stmt),
            Statement::Return(stmt) => write!(f, "{}", stmt),
            Statement::Expr(stmt) => write!(f, "{}", stmt),
            Statement::Block(stmt) => write!(f, "{}", stmt),
            Statement::Nil => write!(f, "nil"),
        }
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(stmt) => stmt.token_literal(),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone)]
pub struct LetStatement {
    pub token: Token,      // token.Let
    pub name: Identifier,  // five
    pub value: Expression, // 5
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier, value: Expression) -> LetStatement {
        LetStatement { token, name, value }
    }
}

#[derive(Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "return {};", self.return_value)
    }
}

impl ReturnStatement {
    pub fn new(token: Token, return_value: Expression) -> ReturnStatement {
        ReturnStatement {
            token,
            return_value,
        }
    }
}

#[derive(Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.expression)
    }
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Expression) -> ExpressionStatement {
        ExpressionStatement { token, expression }
    }
}

#[derive(Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> BlockStatement {
        BlockStatement { token, statements }
    }
}

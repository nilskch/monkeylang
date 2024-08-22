use crate::token::Token;
use std::fmt::{Display, Formatter, Result, Write};

use super::expression::{Expression, Identifier};

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(ExpressionStatement),
    EmptyLine,
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Statement::Let(stmt) => write!(f, "{}", stmt),
            Statement::Return(stmt) => write!(f, "{}", stmt),
            Statement::Expr(stmt) => write!(f, "{}", stmt),
            Statement::EmptyLine => writeln!(f),
        }
    }
}

impl Statement {
    pub fn token_literal(&self) -> &str {
        match self {
            Statement::Let(stmt) => &stmt.token.literal,
            Statement::Return(stmt) => &stmt.token.literal,
            Statement::Expr(stmt) => &stmt.token.literal,
            Statement::EmptyLine => "\n",
        }
    }

    pub fn format(&self, output_buffer: &mut String, depth: usize) {
        match self {
            Statement::Let(stmt) => stmt.format(output_buffer, depth),
            Statement::Return(stmt) => stmt.format(output_buffer, depth),
            Statement::Expr(stmt) => stmt.format(output_buffer, depth),
            Statement::EmptyLine => writeln!(output_buffer).unwrap(),
        };
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct LetStatement {
    pub token: Token,      // token.Let
    pub name: Identifier,  // five
    pub value: Expression, // 5
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

    pub fn format(&self, output_buffer: &mut String, depth: usize) {
        let prefix = "\t".repeat(depth);
        write!(output_buffer, "{}let {} = ", prefix, self.name).unwrap();
        self.value.format(output_buffer, depth);
        writeln!(output_buffer, ";").unwrap();
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
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

    pub fn format(&self, output_buffer: &mut String, depth: usize) {
        let prefix = "\t".repeat(depth);
        write!(output_buffer, "{}return ", prefix).unwrap();
        self.return_value.format(output_buffer, depth);
        writeln!(output_buffer, ";").unwrap()
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
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

    pub fn format(&self, output_buffer: &mut String, depth: usize) {
        let prefix = "\t".repeat(depth);
        write!(output_buffer, "{}", prefix).unwrap();
        self.expression.format(output_buffer, depth);
        match self.expression {
            Expression::IfElse(_) | Expression::Function(_) => {}
            _ => write!(output_buffer, ";").unwrap(),
        }
        writeln!(output_buffer).unwrap();
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
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

    pub fn format(&self, output_buffer: &mut String, depth: usize) {
        for stmt in &self.statements {
            stmt.format(output_buffer, depth + 1);
        }
    }
}

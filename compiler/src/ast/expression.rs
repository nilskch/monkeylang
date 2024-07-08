use super::Node;
use crate::ast::statement::BlockStatement;
use crate::token::Token;
use std::fmt::{Display, Formatter, Result};

#[derive(Clone)]
pub enum Expression {
    Ident(Identifier),
    Integer(IntegerLiteral),
    Boolean(BooleanLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    IfElse(IfExpression),
    Nil,
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self {
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::Integer(integer) => write!(f, "{}", integer),
            Expression::Prefix(prefix_expr) => write!(f, "{}", prefix_expr),
            Expression::Infix(infix_expr) => write!(f, "{}", infix_expr),
            Expression::Boolean(bool_expr) => write!(f, "{}", bool_expr),
            Expression::IfElse(if_expr) => write!(f, "{}", if_expr),
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
            Expression::Infix(infix_expr) => infix_expr.token_literal(),
            Expression::Boolean(bool_expr) => bool_expr.token_literal(),
            Expression::IfElse(if_expr) => if_expr.token_literal(),
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

#[derive(Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl InfixExpression {
    pub fn new(
        token: Token,
        left: Expression,
        operator: String,
        right: Expression,
    ) -> InfixExpression {
        InfixExpression {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

#[derive(Clone)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Node for BooleanLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.token.literal)
    }
}

impl BooleanLiteral {
    pub fn new(token: Token, value: bool) -> BooleanLiteral {
        BooleanLiteral { token, value }
    }
}

#[derive(Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;
        if let Some(alternative) = &self.alternative {
            write!(f, " else {}", alternative)?;
        }
        Ok(())
    }
}

impl IfExpression {
    pub fn new(
        token: Token,
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> IfExpression {
        IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }
    }
}

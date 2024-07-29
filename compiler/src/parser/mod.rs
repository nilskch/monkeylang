mod error;
mod precedence;

use error::ParserError;
use precedence::Precedence;

use crate::ast::expression::{
    ArrayLiteral, BooleanLiteral, CallExpression, Expression, FunctionLiteral, HashLiteral,
    Identifier, IfExpression, Index, InfixExpression, IntegerLiteral, PrefixExpression,
    StringLiteral,
};
use crate::ast::program::Program;
use crate::ast::statement::{
    BlockStatement, ExpressionStatement, LetStatement, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::collections::HashMap;

pub struct Parser {
    pub lexer: Lexer,
    pub cur_token: Token,
    pub peek_token: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        Parser {
            cur_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            errors: vec![],
            lexer,
        }
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut program = Program::new();

        while self.cur_token.token_type != TokenType::Eof {
            if let Ok(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expr(ExpressionStatement::new(token, expression)))
    }

    fn parse_boolean_expression(&self) -> Expression {
        Expression::Boolean(BooleanLiteral::new(
            self.cur_token.clone(),
            self.cur_token_is(TokenType::True),
        ))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        let mut left_expr = match self.cur_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal()?,
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression()?,
            TokenType::True | TokenType::False => self.parse_boolean_expression(),
            TokenType::LParen => self.parse_grouped_expression()?,
            TokenType::If => self.parse_if_expression()?,
            TokenType::Function => self.parse_function_literal()?,
            TokenType::String => self.parse_string_literal(),
            TokenType::LBracket => self.parse_array_literal()?,
            TokenType::LBrace => self.parse_hash_literal()?,
            _ => {
                return Err(ParserError::new(format!(
                    "no prefix parse function for {} found",
                    self.cur_token.token_type.clone()
                )))
            }
        };

        if self.peek_token_is(&TokenType::Semicolon) || precedence >= self.peek_precedence() {
            return Ok(left_expr);
        }

        while !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence() {
            left_expr = match self.peek_token.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterik
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Lt
                | TokenType::Gt
                | TokenType::LtEq
                | TokenType::GtEq => {
                    self.next_token();
                    self.parse_infix_expression(left_expr)?
                }
                TokenType::LParen => {
                    self.next_token();
                    self.parse_call_expression(left_expr)?
                }
                TokenType::LBracket => {
                    self.next_token();
                    self.parse_index_expression(left_expr)?
                }
                _ => return Ok(left_expr),
            };
        }

        Ok(left_expr)
    }

    fn parse_hash_literal(&mut self) -> Result<Expression, ParserError> {
        let token = self.cur_token.clone();
        let mut pairs = HashMap::new();

        while !self.peek_token_is(&TokenType::RBrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;

            self.expect_peek(&TokenType::Colon)?;

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.insert(key, value);

            if !self.peek_token_is(&TokenType::RBrace) {
                self.expect_peek(&TokenType::Comma)?;
            }
        }

        self.expect_peek(&TokenType::RBrace)?;

        Ok(Expression::Hash(HashLiteral::new(token, pairs)))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let token = self.cur_token.clone();
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(&TokenType::RBracket)?;
        Ok(Expression::Index(Index::new(token, left, index)))
    }

    fn parse_array_literal(&mut self) -> Result<Expression, ParserError> {
        let token = self.cur_token.clone();
        let elements = self.parse_expression_list(&TokenType::RBracket)?;
        Ok(Expression::Array(ArrayLiteral::new(token, elements)))
    }

    fn parse_expression_list(&mut self, end: &TokenType) -> Result<Vec<Expression>, ParserError> {
        let mut expressions = vec![];
        if self.peek_token_is(end) {
            self.next_token();
            return Ok(expressions);
        }

        self.next_token();
        expressions.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            expressions.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end)?;

        Ok(expressions)
    }

    fn parse_string_literal(&self) -> Expression {
        Expression::String(StringLiteral::new(
            self.cur_token.clone(),
            self.cur_token.clone().literal,
        ))
    }

    fn parse_identifier(&self) -> Expression {
        let token = self.cur_token.clone();
        let literal = token.literal.clone();
        Expression::Ident(Identifier::new(token, literal))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParserError> {
        let token = self.cur_token.clone();
        let value = match token.literal.parse::<i64>() {
            Ok(value) => value,
            Err(_) => {
                return Err(ParserError::new(format!(
                    "could not parse '{}' to integer",
                    token.literal
                )))
            }
        };

        Ok(Expression::Integer(IntegerLiteral::new(token, value)))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();

        self.next_token();

        let right_expression = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::Prefix(PrefixExpression::new(
            token,
            operator,
            right_expression,
        )))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.cur_token.clone();

        self.expect_peek(&TokenType::Ident)?;

        let name = Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());

        self.expect_peek(&TokenType::Assign)?;

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if !self.cur_token_is(TokenType::Semicolon) {
            self.next_token()
        }

        Ok(Statement::Let(LetStatement::new(token, name, value)))
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.token_type == token_type
    }

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        self.peek_token.token_type == *token_type
    }

    fn expect_peek(&mut self, token_type: &TokenType) -> Result<(), ParserError> {
        if self.peek_token_is(token_type) {
            self.next_token();
            Ok(())
        } else {
            Err(ParserError::new(format!(
                "expected next token to be '{}', but got '{}' instead",
                token_type, self.peek_token.token_type
            )))
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.cur_token.clone();

        self.next_token();
        let return_value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        let return_stmt = ReturnStatement::new(token, return_value);
        Ok(Statement::Return(return_stmt))
    }

    fn peek_precedence(&self) -> Precedence {
        match Precedence::get(&self.peek_token.token_type) {
            Some(precedence) => precedence,
            None => Precedence::Lowest,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match Precedence::get(&self.cur_token.token_type) {
            Some(precedence) => precedence,
            None => Precedence::Lowest,
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();
        let prec = self.cur_precedence();

        self.next_token();

        let right = self.parse_expression(prec)?;
        Ok(Expression::Infix(InfixExpression::new(
            token, left, operator, right,
        )))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest);

        self.expect_peek(&TokenType::RParen)?;
        expr
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self.cur_token.clone();

        self.expect_peek(&TokenType::LParen)?;
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(&TokenType::RParen)?;

        self.expect_peek(&TokenType::LBrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(&TokenType::Else) {
            self.next_token();
            self.expect_peek(&TokenType::LBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::IfElse(IfExpression::new(
            token,
            condition,
            consequence,
            alternative,
        )))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParserError> {
        let token = self.cur_token.clone();
        let mut statements = vec![];

        self.next_token();

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::Eof) {
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt);
            }

            self.next_token();
        }

        Ok(BlockStatement::new(token, statements))
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        let token = self.cur_token.clone();

        self.expect_peek(&TokenType::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(&TokenType::LBrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::Function(FunctionLiteral::new(
            token, parameters, body,
        )))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParserError> {
        let mut identifiers = vec![];

        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        let ident = Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());
        identifiers.push(ident);

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();

            let ident = Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());
            identifiers.push(ident);
        }

        self.expect_peek(&TokenType::RParen)?;

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParserError> {
        let token = self.cur_token.clone();
        let arguments = self.parse_expression_list(&TokenType::RParen)?;

        Ok(Expression::Call(CallExpression::new(
            token, function, arguments,
        )))
    }
}

#[cfg(test)]
mod tests;

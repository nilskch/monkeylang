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
mod tests {
    use super::*;

    enum ExpectedValue {
        Integer(i64),
        String(String),
        Boolean(bool),
    }

    #[test]
    fn test_let_statements() {
        let tests = [
            ("let x = 5;", "x", ExpectedValue::Integer(5)),
            ("let y = true;", "y", ExpectedValue::Boolean(true)),
            (
                "let foobar = y;",
                "foobar",
                ExpectedValue::String(String::from("y")),
            ),
        ];
        for (input, identifier, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            let num_stmt = program.statements.len();
            assert_eq!(
                num_stmt, 1,
                "program.statements.len() wrong. expected={}, got={}",
                1, num_stmt
            );

            let stmt = &program.statements[0];
            test_let_statement(stmt, identifier);

            let let_stmt = match stmt {
                Statement::Let(let_stmt) => let_stmt,
                _ => unreachable!(),
            };

            test_literal_expression(&let_stmt.value, expected)
        }
    }

    fn test_let_statement(stmt: &Statement, name: &str) {
        let token_literal = stmt.token_literal();
        assert_eq!(
            token_literal, "let",
            "stmt.token_literal() not 'let'. got={}",
            token_literal
        );

        let let_stmt = match stmt {
            Statement::Let(stmt) => stmt,
            _ => unreachable!(),
        };

        let stmt_name = &let_stmt.name.value;
        assert_eq!(
            stmt_name, name,
            "let_stmt.name.value not '{}'. got='{}'",
            name, stmt_name
        );
    }

    fn check_parser_errors(parser: Parser) {
        let num_errors = parser.errors.len();

        if num_errors == 0 {
            return;
        }

        println!("parser has {} errors:", num_errors);
        for msg in parser.errors {
            println!("parser error: {}", msg)
        }

        unreachable!();
    }

    #[test]
    fn test_return_statements() {
        let tests = [
            ("return 5;", ExpectedValue::Integer(5)),
            ("return true;", ExpectedValue::Boolean(true)),
            (
                "return foobar;",
                ExpectedValue::String("foobar".to_string()),
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            let num_stmt = program.statements.len();
            assert_eq!(
                num_stmt, 1,
                "program.statements.len() wrong. expected={}, got={}",
                1, num_stmt
            );

            let stmt = &program.statements[0];
            let token_literal = stmt.token_literal();
            assert_eq!(
                token_literal, "return",
                "stmt.token_literal() wrong. wanted='return', got='{}'",
                token_literal,
            );

            let return_stmt = match stmt {
                Statement::Return(stmt) => stmt,
                _ => unreachable!(),
            };

            test_literal_expression(&return_stmt.return_value, expected)
        }
    }

    #[test]
    fn test_indentifier_expression() {
        let input = "foobar";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &program.statements[0];
        let token_literal = stmt.token_literal();
        assert_eq!(
            token_literal, "foobar",
            "token_literal() not 'foobar'. got='{}'",
            token_literal
        );

        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let ident = match &expr_stmt.expression {
            Expression::Ident(ident) => ident,
            _ => unreachable!(),
        };

        assert_eq!(
            ident.value, "foobar",
            "ident.value not 'foobar'. got='{}'",
            ident.value
        );
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &program.statements[0];
        let token_literal = stmt.token_literal();
        assert_eq!(
            token_literal, "5",
            "stmt.token_literal() not 'foobar'. got='{}'",
            token_literal
        );

        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let integer = match &expr_stmt.expression {
            Expression::Integer(integer) => integer,
            _ => unreachable!(),
        };

        assert_eq!(
            integer.value, 5,
            "integer.value not 'foobar'. got='{}'",
            integer.value
        );
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = [
            ("!5;", "!", ExpectedValue::Integer(5)),
            ("-15;", "-", ExpectedValue::Integer(15)),
            ("!true;", "!", ExpectedValue::Boolean(true)),
            ("!false;", "!", ExpectedValue::Boolean(false)),
        ];

        for (input, operator, value) in prefix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            let num_stmts = program.statements.len();
            assert_eq!(
                num_stmts, 1,
                "program.statements.len() wrong. expected={}, got={}",
                1, num_stmts
            );

            let stmt = &program.statements[0];
            let expr_stmt = match stmt {
                Statement::Expr(stmt) => stmt,
                _ => unreachable!(),
            };

            let prefix_expr = match &expr_stmt.expression {
                Expression::Prefix(prefix_expr) => prefix_expr,
                _ => unreachable!(),
            };

            assert_eq!(
                prefix_expr.operator, operator,
                "prefix_expr.operator is not '{}'. got='{}'",
                operator, prefix_expr.operator
            );

            test_literal_expression(&*prefix_expr.right, value);
        }
    }

    fn test_integer_literal(expr: &Expression, value: i64) {
        let token_literal = expr.token_literal();
        let expected_token_literal = value.to_string();
        assert_eq!(
            token_literal, expected_token_literal,
            "integer.token_literal() not '{}'. got=''{}",
            expected_token_literal, token_literal
        );

        let integer = match expr {
            Expression::Integer(integer) => integer,
            _ => unreachable!(),
        };

        assert_eq!(
            integer.value, value,
            "integer.value not {}. got={}",
            value, integer.value
        );
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = [
            (
                "6 + 5;",
                ExpectedValue::Integer(6),
                "+",
                ExpectedValue::Integer(5),
            ),
            (
                "6 - 5;",
                ExpectedValue::Integer(6),
                "-",
                ExpectedValue::Integer(5),
            ),
            (
                "6 * 5;",
                ExpectedValue::Integer(6),
                "*",
                ExpectedValue::Integer(5),
            ),
            (
                "6 / 5;",
                ExpectedValue::Integer(6),
                "/",
                ExpectedValue::Integer(5),
            ),
            (
                "6 > 5;",
                ExpectedValue::Integer(6),
                ">",
                ExpectedValue::Integer(5),
            ),
            (
                "6 < 5;",
                ExpectedValue::Integer(6),
                "<",
                ExpectedValue::Integer(5),
            ),
            (
                "6 == 5;",
                ExpectedValue::Integer(6),
                "==",
                ExpectedValue::Integer(5),
            ),
            (
                "6 != 5;",
                ExpectedValue::Integer(6),
                "!=",
                ExpectedValue::Integer(5),
            ),
            (
                "6 <= 5;",
                ExpectedValue::Integer(6),
                "<=",
                ExpectedValue::Integer(5),
            ),
            (
                "6 >= 5;",
                ExpectedValue::Integer(6),
                ">=",
                ExpectedValue::Integer(5),
            ),
            (
                "true == true",
                ExpectedValue::Boolean(true),
                "==",
                ExpectedValue::Boolean(true),
            ),
            (
                "true != false",
                ExpectedValue::Boolean(true),
                "!=",
                ExpectedValue::Boolean(false),
            ),
            (
                "false == false",
                ExpectedValue::Boolean(false),
                "==",
                ExpectedValue::Boolean(false),
            ),
        ];

        for (input, left_value, operator, right_value) in infix_tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            let num_stmts = program.statements.len();
            assert_eq!(
                num_stmts, 1,
                "program.statements.len() wrong. expected={}, got={}",
                1, num_stmts
            );

            let stmt = &program.statements[0];
            let expr_stmt = match stmt {
                Statement::Expr(stmt) => stmt,
                _ => unreachable!(),
            };

            test_infix_expression(&expr_stmt.expression, left_value, operator, right_value)
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 > 4 != 3 < 4", "((5 > 4) != (3 < 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true;", "true"),
            ("false;", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().unwrap();
            check_parser_errors(parser);

            let actual = program.to_string();
            assert_eq!(
                actual, expected,
                "expected='{}'. got='{}'",
                expected, actual
            );
        }
    }

    fn test_identifier(expr: &Expression, value: String) {
        let token_literal = expr.token_literal();
        assert_eq!(
            token_literal, value,
            "token_literal() not '{}', got='{}'",
            token_literal, value
        );

        let ident = match expr {
            Expression::Ident(ident) => ident,
            _ => unreachable!(),
        };

        assert_eq!(
            ident.value, value,
            "ident.value not '{}', got='{}'",
            value, ident.value
        );
    }

    fn test_literal_expression(expr: &Expression, expected: ExpectedValue) {
        match expected {
            ExpectedValue::Integer(value) => test_integer_literal(expr, value),
            ExpectedValue::String(value) => test_identifier(expr, value),
            ExpectedValue::Boolean(value) => test_boolean_literal(expr, value),
        }
    }

    fn test_boolean_literal(expr: &Expression, value: bool) {
        let token_literal = expr.token_literal();
        let expected = format!("{}", value);
        assert_eq!(
            token_literal, expected,
            "token_literal() not '{}', got='{}'",
            expected, token_literal
        );

        let boolean_expr = match expr {
            Expression::Boolean(boolean_expr) => boolean_expr,
            _ => unreachable!(),
        };

        assert_eq!(
            boolean_expr.value, value,
            "boolean_expr.value not '{}'. got='{}'",
            value, boolean_expr.value
        );
    }

    fn test_infix_expression(
        expr: &Expression,
        left: ExpectedValue,
        operator: &str,
        right: ExpectedValue,
    ) {
        let infix_expr = match expr {
            Expression::Infix(infix_expr) => infix_expr,
            _ => unreachable!(),
        };

        test_literal_expression(&*infix_expr.left, left);
        assert_eq!(
            infix_expr.operator, operator,
            "infix_expr.operator not '{}'. got='{}'",
            operator, infix_expr.operator
        );

        test_literal_expression(&*infix_expr.right, right);
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let if_expr = match &expr_stmt.expression {
            Expression::IfElse(if_expr) => if_expr,
            _ => unreachable!(),
        };

        test_infix_expression(
            &*if_expr.condition,
            ExpectedValue::String("x".to_string()),
            "<",
            ExpectedValue::String("y".to_string()),
        );

        let num_stmts = if_expr.consequence.statements.len();
        assert_eq!(
            num_stmts, 1,
            "if_expr.consequence.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &if_expr.consequence.statements[0];
        let consequence = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        test_identifier(&consequence.expression, "x".to_string());

        if let Some(_) = if_expr.alternative {
            unreachable!();
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let if_expr = match &expr_stmt.expression {
            Expression::IfElse(if_expr) => if_expr,
            _ => unreachable!(),
        };

        test_infix_expression(
            &*if_expr.condition,
            ExpectedValue::String(String::from("x")),
            "<",
            ExpectedValue::String(String::from("y")),
        );

        let num_stmts = if_expr.consequence.statements.len();
        assert_eq!(
            num_stmts, 1,
            "if_expr.consequence.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &if_expr.consequence.statements[0];
        let consequence = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        test_identifier(&consequence.expression, "x".to_string());

        let alternative = match &if_expr.alternative {
            Some(alternative) => alternative,
            None => unreachable!(),
        };

        let num_stmts = alternative.statements.len();
        assert_eq!(
            num_stmts, 1,
            "alternative.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &alternative.statements[0];
        let alternative = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        test_identifier(&alternative.expression, String::from("y"));
    }

    #[test]
    fn test_parsing_function_literal() {
        let input = "fn(x, y) {x + y;}";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let func_literal = match &expr_stmt.expression {
            Expression::Function(func_literal) => func_literal,
            _ => unreachable!(),
        };

        assert_eq!(
            func_literal.parameters.len(),
            2,
            "func_literal.parameters.len() not '2'. got='{}'",
            func_literal.parameters.len()
        );

        test_identifier(
            &Expression::Ident(func_literal.parameters[0].clone()),
            String::from("x"),
        );
        test_identifier(
            &Expression::Ident(func_literal.parameters[1].clone()),
            String::from("y"),
        );

        assert_eq!(
            func_literal.body.statements.len(),
            1,
            "func_literal.body.statements.len() not '1'. got='{}'",
            func_literal.body.statements.len()
        );

        let stmt = &func_literal.body.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        test_infix_expression(
            &expr_stmt.expression,
            ExpectedValue::String(String::from("x")),
            "+",
            ExpectedValue::String(String::from("y")),
        )
    }

    #[test]
    fn test_object_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5)";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let object_expr = match &expr_stmt.expression {
            Expression::Call(object_expr) => object_expr,
            _ => unreachable!(),
        };

        test_identifier(&*object_expr.function, String::from("add"));
        assert_eq!(
            object_expr.arguments.len(),
            3,
            "Object_expr.arguments.len() not '3'. got='{}'",
            object_expr.arguments.len()
        );

        test_literal_expression(&object_expr.arguments[0], ExpectedValue::Integer(1));
        test_infix_expression(
            &object_expr.arguments[1],
            ExpectedValue::Integer(2),
            "*",
            ExpectedValue::Integer(3),
        );
        test_infix_expression(
            &object_expr.arguments[2],
            ExpectedValue::Integer(4),
            "+",
            ExpectedValue::Integer(5),
        );
    }

    #[test]
    fn test_parsing_string_literal() {
        let input = "\"hello world\"";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let string_literal = match &expr_stmt.expression {
            Expression::String(string_literal) => string_literal,
            _ => unreachable!(),
        };

        let expected = "hello world";
        assert_eq!(
            string_literal.value, expected,
            "invalid string literal. expected='{}'. got='{}'",
            expected, string_literal.value
        );
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let array_literal = match &expr_stmt.expression {
            Expression::Array(array_literal) => array_literal,
            _ => unreachable!(),
        };

        assert_eq!(
            array_literal.elements.len(),
            3,
            "len(arr.elements) not 3. got={}",
            array_literal.elements.len()
        );

        test_integer_literal(&array_literal.elements[0], 1);
        test_infix_expression(
            &array_literal.elements[1],
            ExpectedValue::Integer(2),
            "*",
            ExpectedValue::Integer(2),
        );
        test_infix_expression(
            &array_literal.elements[2],
            ExpectedValue::Integer(3),
            "+",
            ExpectedValue::Integer(3),
        );
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let index_expr = match &expr_stmt.expression {
            Expression::Index(index) => index,
            _ => unreachable!(),
        };

        test_identifier(&index_expr.left, String::from("myArray"));
        test_infix_expression(
            &index_expr.index,
            ExpectedValue::Integer(1),
            "+",
            ExpectedValue::Integer(1),
        );
    }

    #[test]
    fn test_parsing_hash_literal_string_keys() {
        let input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let hash_expr = match &expr_stmt.expression {
            Expression::Hash(hash_expr) => hash_expr,
            _ => unreachable!(),
        };

        assert_eq!(
            hash_expr.pairs.len(),
            3,
            "hash.pairs has wrong length. want=3. got={}",
            hash_expr.pairs.len()
        );

        let mut expected = HashMap::new();
        expected.insert(String::from("one"), 1 as i64);
        expected.insert(String::from("two"), 2 as i64);
        expected.insert(String::from("three"), 3 as i64);

        for (key, value) in hash_expr.pairs.iter() {
            let string_literal = match key {
                Expression::String(string_literal) => string_literal,
                _ => unreachable!(),
            };

            let expected_value = match expected.get(&string_literal.value) {
                Some(val) => *val,
                None => unreachable!(),
            };

            test_integer_literal(value, expected_value)
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let hash_expr = match &expr_stmt.expression {
            Expression::Hash(hash_expr) => hash_expr,
            _ => unreachable!(),
        };

        assert_eq!(
            hash_expr.pairs.len(),
            0,
            "hash.pairs has wrong length. want=0. got={}",
            hash_expr.pairs.len()
        );
    }

    #[test]
    fn test_parsing_hash_literal_with_expression() {
        let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        check_parser_errors(parser);

        let stmt = &program.statements[0];
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let hash_expr = match &expr_stmt.expression {
            Expression::Hash(hash_expr) => hash_expr,
            _ => unreachable!(),
        };

        assert_eq!(
            hash_expr.pairs.len(),
            3,
            "hash.pairs has wrong length. want=3. got={}",
            hash_expr.pairs.len()
        );

        for (key, value) in hash_expr.pairs.iter() {
            let string_literal = match key {
                Expression::String(string_literal) => string_literal,
                _ => unreachable!(),
            };

            match &*string_literal.value {
                "one" => test_infix_expression(
                    value,
                    ExpectedValue::Integer(0),
                    "+",
                    ExpectedValue::Integer(1),
                ),
                "two" => test_infix_expression(
                    value,
                    ExpectedValue::Integer(10),
                    "-",
                    ExpectedValue::Integer(8),
                ),
                "three" => test_infix_expression(
                    value,
                    ExpectedValue::Integer(15),
                    "/",
                    ExpectedValue::Integer(5),
                ),
                _ => unreachable!(),
            }
        }
    }
}

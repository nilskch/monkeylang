mod precedence;

use precedence::PRECEDENCES;

use self::precedence::Precedence;
use crate::ast::expression::{
    BooleanLiteral, Expression, FunctionLiteral, Identifier, IfExpression, InfixExpression,
    IntegerLiteral, PrefixExpression,
};
use crate::ast::program::Program;
use crate::ast::statement::{
    BlockStatement, ExpressionStatement, LetStatement, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

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

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token.token_type != TokenType::Eof {
            if let Ok(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement, ()> {
        match self.cur_token.token_type {
            TokenType::Let => Ok(self.parse_let_statement()),
            TokenType::Return => Ok(self.parse_return_statement()),
            _ => Ok(self.parse_expression_statement()),
        }
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Statement::Expr(ExpressionStatement::new(token, expression))
    }

    fn parse_boolean_expression(&self) -> Expression {
        Expression::Boolean(BooleanLiteral::new(
            self.cur_token.clone(),
            self.cur_token_is(TokenType::True),
        ))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expression {
        let mut left_expr = match self.cur_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            TokenType::True | TokenType::False => self.parse_boolean_expression(),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(),
            TokenType::Fucntion => self.parse_function_literal(),
            _ => {
                self.no_prefix_parse_fn_error(self.cur_token.token_type.clone());
                return Expression::Nil;
            }
        };

        if self.peek_token_is(TokenType::Semicolon) || precedence >= self.peek_precedence() {
            return left_expr;
        }

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            left_expr = match self.peek_token.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterik
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Lt
                | TokenType::Gt => {
                    self.next_token();
                    self.parse_infix_expression(left_expr)
                }
                _ => return left_expr,
            };
        }

        left_expr
    }

    fn parse_identifier(&self) -> Expression {
        let token = self.cur_token.clone();
        let literal = token.literal.clone();
        Expression::Ident(Identifier::new(token, literal))
    }

    fn parse_integer_literal(&mut self) -> Expression {
        let token = self.cur_token.clone();
        let value = match token.literal.parse::<i64>() {
            Ok(value) => value,
            Err(_) => {
                let msg = format!("could not parse '{}' to integer", token.literal);
                self.errors.push(msg);
                return Expression::Nil;
            }
        };

        Expression::Integer(IntegerLiteral::new(token, value))
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();

        self.next_token();

        let right_expression = self.parse_expression(Precedence::Prefix);

        Expression::Prefix(PrefixExpression::new(token, operator, right_expression))
    }

    fn parse_let_statement(&mut self) -> Statement {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return Statement::Nil;
        }

        let name = Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());

        if !self.expect_peek(TokenType::Assign) {
            return Statement::Nil;
        }

        // skip expression until we encouter semicolon
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token()
        }

        Statement::Let(LetStatement::new(token, name, Expression::Nil))
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.token_type == token_type
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token.token_type == token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(token_type.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(&token_type);
            false
        }
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token_type: &TokenType) {
        let msg = format!(
            "expected next token to be '{}', but got '{}' instead",
            token_type, self.peek_token.token_type
        );
        self.errors.push(msg);
    }

    fn parse_return_statement(&mut self) -> Statement {
        let token = self.cur_token.clone();

        self.next_token();
        let return_value = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        let return_stmt = ReturnStatement::new(token, return_value);
        Statement::Return(return_stmt)
    }

    fn no_prefix_parse_fn_error(&mut self, token_type: TokenType) {
        let msg = format!("no prefix parse function for {} found", token_type);
        self.errors.push(msg);
    }

    fn peek_precedence(&self) -> Precedence {
        match PRECEDENCES.get(&self.peek_token.token_type) {
            Some(precedence) => precedence.clone(),
            None => Precedence::Lowest,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match PRECEDENCES.get(&self.cur_token.token_type) {
            Some(precedence) => precedence.clone(),
            None => Precedence::Lowest,
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();
        let prec = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(prec);

        Expression::Infix(InfixExpression::new(token, left, operator, right))
    }

    fn parse_grouped_expression(&mut self) -> Expression {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::RParen) {
            Expression::Nil
        } else {
            expr
        }
    }

    fn parse_if_expression(&mut self) -> Expression {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return Expression::Nil;
        }

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::RParen) {
            return Expression::Nil;
        }

        if !self.expect_peek(TokenType::LBrace) {
            return Expression::Nil;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(TokenType::Else) {
            self.next_token();

            if !self.expect_peek(TokenType::LBrace) {
                None
            } else {
                Some(self.parse_block_statement())
            }
        } else {
            None
        };

        Expression::IfElse(IfExpression::new(
            token,
            condition,
            consequence,
            alternative,
        ))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = self.cur_token.clone();
        let mut statements = vec![];

        self.next_token();

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::Eof) {
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt);
            }

            self.next_token();
        }

        BlockStatement::new(token, statements)
    }

    fn parse_function_literal(&mut self) -> Expression {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return Expression::Nil;
        }

        let parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenType::LBrace) {
            return Expression::Nil;
        }

        let body = self.parse_block_statement();

        Expression::Function(FunctionLiteral::new(token, parameters, body))
    }

    fn parse_function_parameters(&mut self) -> Vec<Expression> {
        let mut identifiers = vec![];

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return identifiers;
        }

        self.next_token();

        let ident = Expression::Ident(Identifier::new(
            self.cur_token.clone(),
            self.cur_token.literal.clone(),
        ));
        identifiers.push(ident);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            let ident = Expression::Ident(Identifier::new(
                self.cur_token.clone(),
                self.cur_token.literal.clone(),
            ));
            identifiers.push(ident);
        }

        if !self.expect_peek(TokenType::RParen) {
            return vec![];
        }

        identifiers
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Node;

    enum ExpectedValue {
        Integer(i64),
        String(String),
        Boolean(bool),
    }

    #[test]
    fn test_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        let num_stmt = program.statements.len();
        assert_eq!(
            num_stmt, 3,
            "program.statements.len() wrong. expected={}, got={}",
            3, num_stmt
        );

        let tests = ["x", "y", "foobar"];

        for (i, &expected) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            test_let_statement(stmt, expected);
        }
    }

    fn test_let_statement(stmt: &Statement, name: &str) {
        assert_eq!(
            stmt.token_literal(),
            "let",
            "stmt.token_literal() not 'let'. got={}",
            stmt.token_literal()
        );

        let let_stmt = match stmt {
            Statement::Let(stmt) => stmt,
            _ => unreachable!(),
        };

        let stmt_name = let_stmt.name.value.as_str();
        assert_eq!(
            stmt_name, name,
            "let_stmt.name.value not '{}'. got='{}'",
            name, stmt_name
        );

        let token_literal = let_stmt.name.token_literal();
        assert_eq!(
            token_literal, name,
            "let_stmt.name.token_literal() not '{}'. got='{}'",
            name, token_literal
        )
    }

    fn check_parser_errors(parser: Parser) {
        let errors = parser.errors();
        let num_errors = errors.len();

        if num_errors == 0 {
            return;
        }

        println!("parser has {} errors:", num_errors);
        for msg in errors {
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
            let program = parser.parse_program();
            check_parser_errors(parser);

            let num_stmt = program.statements.len();
            assert_eq!(
                num_stmt, 1,
                "program.statements.len() wrong. expected={}, got={}",
                1, num_stmt
            );

            let stmt = program.statements[0].clone();
            let return_stmt = match stmt {
                Statement::Return(stmt) => stmt,
                _ => unreachable!(),
            };

            let token_literal = return_stmt.token_literal();
            assert_eq!(
                token_literal, "return",
                "return_stmt.token_literal() wrong. wanted='return', got='{}'",
                token_literal,
            );

            test_literal_expression(return_stmt.return_value, expected)
        }
    }

    #[test]
    fn test_indentifier_expression() {
        let input = "foobar";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = program.statements[0].clone();
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let ident = match expr_stmt.expression {
            Expression::Ident(ident) => ident,
            _ => unreachable!(),
        };

        assert_eq!(
            ident.value, "foobar",
            "ident.value not 'foobar'. got='{}'",
            ident.value
        );

        let token_literal = ident.token_literal();
        assert_eq!(
            token_literal, "foobar",
            "ident.token_literal() not 'foobar'. got='{}'",
            token_literal
        );
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = program.statements[0].clone();
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let integer = match expr_stmt.expression {
            Expression::Integer(integer) => integer,
            _ => unreachable!(),
        };

        assert_eq!(
            integer.value, 5,
            "integer.value not 'foobar'. got='{}'",
            integer.value
        );

        let token_literal = integer.token_literal();
        assert_eq!(
            token_literal, "5",
            "integer.token_literal() not 'foobar'. got='{}'",
            token_literal
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
            let program = parser.parse_program();
            check_parser_errors(parser);

            let num_stmts = program.statements.len();
            assert_eq!(
                num_stmts, 1,
                "program.statements.len() wrong. expected={}, got={}",
                1, num_stmts
            );

            let stmt = program.statements[0].clone();
            let expr_stmt = match stmt {
                Statement::Expr(stmt) => stmt,
                _ => unreachable!(),
            };

            let prefix_expr = match expr_stmt.expression {
                Expression::Prefix(prefix_expr) => prefix_expr,
                _ => unreachable!(),
            };

            assert_eq!(
                prefix_expr.operator, operator,
                "prefix_expr.operator is not '{}'. got='{}'",
                operator, prefix_expr.operator
            );

            test_literal_expression(*prefix_expr.right, value);
        }
    }

    fn test_integer_literal(expr: Expression, value: i64) {
        let integer = match expr {
            Expression::Integer(integer) => integer,
            _ => unreachable!(),
        };

        assert_eq!(
            integer.value, value,
            "integer.value not {}. got={}",
            value, integer.value
        );

        let token_literal = integer.token_literal();
        let expected_token_literal = value.to_string();
        assert_eq!(
            token_literal, expected_token_literal,
            "integer.token_literal() not '{}'. got=''{}",
            expected_token_literal, token_literal
        )
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
            let program = parser.parse_program();
            check_parser_errors(parser);

            let num_stmts = program.statements.len();
            assert_eq!(
                num_stmts, 1,
                "program.statements.len() wrong. expected={}, got={}",
                1, num_stmts
            );

            let stmt = program.statements[0].clone();
            let expr_stmt = match stmt {
                Statement::Expr(stmt) => stmt,
                _ => unreachable!(),
            };

            test_infix_expression(expr_stmt.expression, left_value, operator, right_value)
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
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parser_errors(parser);

            let actual = program.to_string();
            assert_eq!(
                actual, expected,
                "expected='{}'. got='{}'",
                expected, actual
            );
        }
    }

    fn test_identifier(expr: Expression, value: String) {
        let ident = match expr {
            Expression::Ident(ident) => ident,
            _ => unreachable!(),
        };

        assert_eq!(
            ident.value, value,
            "ident.value not '{}', got='{}'",
            value, ident.value
        );

        let token_literal = ident.token_literal();
        assert_eq!(
            token_literal, value,
            "ident.token_literal() not '{}', got='{}'",
            token_literal, value
        );
    }

    fn test_literal_expression(expr: Expression, expected: ExpectedValue) {
        match expected {
            ExpectedValue::Integer(value) => test_integer_literal(expr, value),
            ExpectedValue::String(value) => test_identifier(expr, value),
            ExpectedValue::Boolean(value) => test_boolean_literal(expr, value),
        }
    }

    fn test_boolean_literal(expr: Expression, value: bool) {
        let boolean_expr = match expr {
            Expression::Boolean(boolean_expr) => boolean_expr,
            _ => unreachable!(),
        };

        assert_eq!(
            boolean_expr.value, value,
            "boolean_expr.value not '{}'. got='{}'",
            value, boolean_expr.value
        );

        let token_literal = boolean_expr.token_literal();
        let expected = format!("{}", value);
        assert_eq!(
            token_literal, expected,
            "boolean_expr.token_literal() not '{}', got='{}'",
            expected, token_literal
        );
    }

    fn test_infix_expression(
        expr: Expression,
        left: ExpectedValue,
        operator: &str,
        right: ExpectedValue,
    ) {
        let infix_expr = match expr {
            Expression::Infix(infix_expr) => infix_expr,
            _ => unreachable!(),
        };

        test_literal_expression(*infix_expr.left, left);
        assert_eq!(
            infix_expr.operator, operator,
            "infix_expr.operator not '{}'. got='{}'",
            operator, infix_expr.operator
        );

        test_literal_expression(*infix_expr.right, right);
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = program.statements[0].clone();
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let if_expr = match expr_stmt.expression {
            Expression::IfElse(if_expr) => if_expr,
            _ => unreachable!(),
        };

        test_infix_expression(
            *if_expr.condition,
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

        let stmt = if_expr.consequence.statements[0].clone();
        let consequence = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        test_identifier(consequence.expression, "x".to_string());

        if let Some(_) = if_expr.alternative {
            unreachable!();
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = program.statements[0].clone();
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let if_expr = match expr_stmt.expression {
            Expression::IfElse(if_expr) => if_expr,
            _ => unreachable!(),
        };

        test_infix_expression(
            *if_expr.condition,
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

        let stmt = if_expr.consequence.statements[0].clone();
        let consequence = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        test_identifier(consequence.expression, "x".to_string());

        let alternative = match if_expr.alternative {
            Some(alternative) => alternative,
            None => unreachable!(),
        };

        let num_stmts = alternative.statements.len();
        assert_eq!(
            num_stmts, 1,
            "alternative.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = alternative.statements[0].clone();
        let alternative = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        test_identifier(alternative.expression, "y".to_string());
    }

    #[test]
    fn test_parsing_function_literal() {
        let input = "fn(x, y) {x + y;}";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(parser);

        let num_stmts = program.statements.len();
        assert_eq!(
            num_stmts, 1,
            "program.statements.len() wrong. expected={}, got={}",
            1, num_stmts
        );

        let stmt = program.statements[0].clone();
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        let func_literal = match expr_stmt.expression {
            Expression::Function(func_literal) => func_literal,
            _ => unreachable!(),
        };

        assert_eq!(
            func_literal.parameters.len(),
            2,
            "func_literal.parameters.len() not '2'. got='{}'",
            func_literal.parameters.len()
        );

        test_literal_expression(
            func_literal.parameters[0].clone(),
            ExpectedValue::String("x".to_string()),
        );
        test_literal_expression(
            func_literal.parameters[1].clone(),
            ExpectedValue::String("y".to_string()),
        );

        assert_eq!(
            func_literal.body.statements.len(),
            1,
            "func_literal.body.statements.len() not '1'. got='{}'",
            func_literal.body.statements.len()
        );

        let stmt = func_literal.body.statements[0].clone();
        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        test_infix_expression(
            expr_stmt.expression,
            ExpectedValue::String("x".to_string()),
            "+",
            ExpectedValue::String("y".to_string()),
        )
    }
}

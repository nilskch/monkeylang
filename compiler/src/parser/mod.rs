mod precedence;

use self::precedence::Precedence;
use crate::ast::expression::{Expression, Identifier, IntegerLiteral};
use crate::ast::program::Program;
use crate::ast::statement::{ExpressionStatement, LetStatement, ReturnStatement, Statement};
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

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Statement::Expr(ExpressionStatement::new(token, expression))
    }

    fn parse_expression(&mut self, _precedence: Precedence) -> Expression {
        let left_expr = match self.cur_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            _ => return Expression::Nil,
        };

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

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        self.peek_token.token_type == *token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token_is(&token_type) {
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
        // TODO: parse correct return_value from expression
        let return_value = Expression::Nil;

        self.next_token();
        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        let return_stmt = ReturnStatement::new(token, return_value);
        Statement::Return(return_stmt)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Node;

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

        assert!(matches!(stmt, Statement::Let(_)));
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
        let input = "
            return 5;
            return 10;
            return 993322;
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

        for stmt in program.statements {
            assert!(matches!(stmt, Statement::Return(_)));
            let return_stmt = match stmt {
                Statement::Return(stmt) => stmt,
                _ => unreachable!(),
            };

            let token_literal = return_stmt.token_literal();
            assert_eq!(
                token_literal, "return",
                "return_stmt.token_literal() wrong. wanted='return', got='{}'",
                token_literal,
            )
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
        assert!(matches!(stmt, Statement::Expr(_)));

        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        assert!(matches!(expr_stmt.expression, Expression::Ident(_)));

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
        assert!(matches!(stmt, Statement::Expr(_)),);

        let expr_stmt = match stmt {
            Statement::Expr(stmt) => stmt,
            _ => unreachable!(),
        };

        assert!(matches!(expr_stmt.expression, Expression::Integer(_)));

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
}

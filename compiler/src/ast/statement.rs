use crate::{ast::Node, token::Token};
use std::fmt::{Display, Formatter, Result};

use super::expression::{Expression, Identifier};

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Nil,
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self {
            Statement::Let(stmt) => write!(f, "let = {} = {};", stmt.name, stmt.value),
            Statement::Return(stmt) => write!(f, "return {};", stmt.return_value),
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

pub struct LetStatement {
    token: Token,      // token.Let
    name: Identifier,  // five
    value: Expression, // 5
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier) -> LetStatement {
        LetStatement {
            token,
            name,
            value: Expression::Empty,
        }
    }
}

pub struct ReturnStatement {
    token: Token,
    return_value: Expression,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

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
}

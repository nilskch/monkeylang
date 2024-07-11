pub mod expression;
pub mod program;
pub mod statement;

use crate::ast::expression::Expression;
use crate::ast::program::Program;
use crate::ast::statement::Statement;

pub enum Node {
    Program(Program),
    Expression(Expression),
    Statement(Statement),
}

#[cfg(test)]
mod tests {
    use expression::Expression;
    use expression::Identifier;
    use program::Program;
    use statement::{LetStatement, Statement};

    use crate::{token::Token, token::TokenType};

    use super::*;

    #[test]
    fn test_fmt() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement::new(
                Token::new(TokenType::Let, String::from("let")),
                Identifier::new(
                    Token::new(TokenType::Ident, String::from("myVar")),
                    String::from("myVar"),
                ),
                Expression::Ident(Identifier::new(
                    Token::new(TokenType::Ident, String::from("anotherVar")),
                    String::from("anotherVar"),
                )),
            ))],
        };
        let program = format!("{}", program);
        assert_eq!(program, "let myVar = anotherVar;");
    }
}

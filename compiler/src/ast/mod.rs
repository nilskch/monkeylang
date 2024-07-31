pub mod expression;
pub mod program;
pub mod statement;

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
                Token::new_without_pos(TokenType::Let, String::from("let")),
                Identifier::new(
                    Token::new_without_pos(TokenType::Ident, String::from("myVar")),
                    String::from("myVar"),
                ),
                Expression::Ident(Identifier::new(
                    Token::new_without_pos(TokenType::Ident, String::from("anotherVar")),
                    String::from("anotherVar"),
                )),
            ))],
        };
        let program = format!("{}", program);
        assert_eq!(program, "let myVar = anotherVar;");
    }
}

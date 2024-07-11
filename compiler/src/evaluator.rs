use crate::ast::expression::Expression;
use crate::ast::statement::Statement;
use crate::ast::Node;
use crate::object::Object;

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(program) => eval_statements(program.statements),
        Node::Expression(expr) => eval_expression(expr),
        Node::Statement(stmt) => eval_statement(stmt),
    }
}

fn eval_statements(stmts: Vec<Statement>) -> Object {
    let mut result = Object::Null;

    for stmt in stmts {
        result = eval(Node::Statement(stmt));
    }

    result
}

fn eval_statement(stmt: Statement) -> Object {
    match stmt {
        Statement::Expr(expr_stmt) => eval_expression(expr_stmt.expression),
        _ => unreachable!(),
    }
}

fn eval_expression(expr: Expression) -> Object {
    match expr {
        Expression::Integer(integer_literal) => Object::Integer(integer_literal.value),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::Lexer, object::Object, parser::Parser};

    #[test]
    fn test_eval_integer_expression() {
        let tests = [("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval(Node::Program(program))
    }

    fn test_integer_object(object: Object, expected: i64) {
        let integer_value = match object {
            Object::Integer(integer_value) => integer_value,
            _ => unreachable!(),
        };

        assert_eq!(
            integer_value, expected,
            "object has wrong value. got='{}'. wanted='{}'",
            integer_value, expected
        );
    }
}

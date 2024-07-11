use crate::ast::expression::{Expression, IfExpression};
use crate::ast::statement::Statement;
use crate::ast::Node;
use crate::object::{Object, BOOLEAN_OBJ, INTEGER_OBJ};

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
        _ => Object::Null,
    }
}

fn eval_expression(expr: Expression) -> Object {
    match expr {
        Expression::Integer(integer_literal) => Object::Integer(integer_literal.value),
        Expression::Boolean(boolean_litereal) => Object::Boolean(boolean_litereal.value),
        Expression::Prefix(prefix_expr) => {
            let right = eval(Node::Expression(*prefix_expr.right));
            eval_prefix_expression(&prefix_expr.operator, right)
        }
        Expression::Infix(infix_expr) => {
            let left = eval(Node::Expression(*infix_expr.left));
            let right = eval(Node::Expression(*infix_expr.right));
            eval_infix_expression(&infix_expr.operator, left, right)
        }
        Expression::IfElse(if_expr) => eval_if_expression(if_expr),
        _ => Object::Null,
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Null,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-value),
        _ => Object::Null,
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(value) => Object::Boolean(!value),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    if left.object_type() != right.object_type() {
        return Object::Null;
    }
    match left.object_type() {
        INTEGER_OBJ => eval_integer_infix_expression(operator, left, right),
        BOOLEAN_OBJ => eval_boolean_infix_expression(operator, left, right),
        _ => Object::Null,
    }
}

fn eval_boolean_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    let left = match left {
        Object::Boolean(value) => value,
        _ => return Object::Null,
    };
    let right = match right {
        Object::Boolean(value) => value,
        _ => return Object::Null,
    };

    match operator {
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Null,
    }
}

fn eval_integer_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    let left = match left {
        Object::Integer(value) => value,
        _ => return Object::Null,
    };
    let right = match right {
        Object::Integer(value) => value,
        _ => return Object::Null,
    };

    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "<=" => Object::Boolean(left <= right),
        ">=" => Object::Boolean(left >= right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Null,
    }
}

fn eval_if_expression(if_expr: IfExpression) -> Object {
    let condition = eval_expression(*if_expr.condition);

    if is_truthy(condition) {
        return eval_statements(if_expr.consequence.statements);
    }
    if let Some(alternative) = if_expr.alternative {
        return eval_statements(alternative.statements);
    }

    Object::Null
}

fn is_truthy(object: Object) -> bool {
    match object {
        Object::Null => false,
        Object::Boolean(val) => val,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lexer::Lexer,
        object::{Object, NULL_OBJ},
        parser::Parser,
    };

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

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

    #[test]
    fn test_boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("1 <= 2", true),
            ("1 >= 2", false),
            ("2 <= 2", true),
            ("2 >= 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    fn test_boolean_object(object: Object, expected: bool) {
        let boolean_value = match object {
            Object::Boolean(boolean_value) => boolean_value,
            _ => unreachable!(),
        };

        assert_eq!(
            boolean_value, expected,
            "object has wrong value. got='{}'. wanted='{}'",
            boolean_value, expected
        );
    }

    #[test]
    fn test_bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = [
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_object(evaluated, expected);
        }
    }

    fn test_object(object: Object, expected: Object) {
        let object_type = object.object_type();
        let expected_type = expected.object_type();

        assert_eq!(
            object_type, expected_type,
            "object.object_type() not '{}'. got='{}'",
            expected_type, object_type
        );

        match expected {
            Object::Integer(value) => test_integer_object(object, value),
            Object::Null => test_null_object(object),
            _ => unreachable!(),
        }
    }

    fn test_null_object(object: Object) {
        let object_type = object.object_type();
        assert_eq!(
            object_type, NULL_OBJ,
            "object.object_type() not '{}'. got='{}'",
            NULL_OBJ, object_type
        );
    }
}
